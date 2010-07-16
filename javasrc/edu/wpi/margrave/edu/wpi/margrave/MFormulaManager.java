/*
  	Copyright 2009-2010 Brown University and Worcester Polytechnic Institute.
    
    This file is part of Margrave.

    Margrave is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Margrave is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Margrave.  If not, see <http://www.gnu.org/licenses/>.
 */



/**
 * Arbitrates allocation of KodKod Formula objects. 
 * 
 * Used to avoid non-equal yet isomorphic formula objects:
 * 
 * Variable x = Variable.unary("x");
 * Relation R = Relation.unary("R");
 * Formula f1 = x.in(R);
 * Formula f2 = x.in(R);
 * // f1 and f2 will be different instances!
 *  
 * 
 * @author Tim
 *
 */

package edu.wpi.margrave;

import java.util.*;
import java.lang.management.*;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import kodkod.ast.*;
import kodkod.ast.operator.FormulaOperator;
import kodkod.ast.operator.Multiplicity;
import kodkod.ast.operator.Quantifier;

// This may behave oddly (appear not to free formulas) in Eclipse, but when run outside
// of Eclipse it works fine...


/**
 * Indicates an error given by the Formula Manager:
 * Unknown Relation name, etc.
 * @author Tim
 *
 */
class MGEManagerException extends MGException
{
	private static final long serialVersionUID = 100; 
	
	MGEManagerException(String s)
	{
		super(s);
	}
}

class MWeakValue<T> extends WeakReference<T>
{
	// Some of this code due to Apache's Markus Fuchs.
		
	// The value must leave a trail of breadcrumbs back to its key,
	// lest it get lost in the enchanted Garbage Collection forest
	// and be eaten by a grue.

	private Object key;
	
	MWeakValue(Object key, T val, ReferenceQueue<T> rqueue)
	{
		super(val, rqueue);
		this.key = key;
	}
	
	public boolean equals(Object other)
	{
		if (this == other)
            return true;

        if (!(other instanceof MWeakValue))
            return false;

        Object ref1 = this.get();
        Object ref2 = ((MWeakValue<T>) other).get();

        if (ref1 == ref2)
            return true;

        if ((ref1 == null) || (ref2 == null))
            return false;

        return ref1.equals(ref2);
	}

	public int hashCode()
	{
		T ref = this.get();
		if(ref == null)
			return 0;
		return ref.hashCode();
	}
	
	public Object getKey()
	{
		return key;
	}
	
	
}


class MSetWrapper<T>
{
	private Set<T> internalSet;
	private int myHashCode;
	
	
	MSetWrapper(Set<T> internalSet)
	{
		this.internalSet = internalSet;
		
		// set hash code EXACTLY ONCE
		myHashCode = internalSet.hashCode();
	}
	
	public int hashCode()
	{
		return myHashCode;
	}
	
	public boolean equals(Object other)
	{
		if(this == other)
			return true;
		
		if(! (other instanceof MSetWrapper<?>))
			return false;
		
		// Let the internal sets decide
		return ((MSetWrapper<T>) other).internalSet.equals(this.internalSet);
	}
}


class MWeakArrayVector<T>
{
	private ReferenceQueue<T> gced = new ReferenceQueue<T>();
	private final ArrayList<WeakReference<T>> internalList; // can't make a generic array
	private int myHashCode = 0;
	
	MWeakArrayVector(int size)
	{
		// Fix the length of the list!
		internalList = new ArrayList<WeakReference<T>>(size);
		// populate the elements properly
		for(int iIndex =0; iIndex <size; iIndex++)
			internalList.add(null);
	}
	
	public int reCalculateHashCode()
	{
		// xor the hashCodes of element referents
		int result = 0;
		for(WeakReference<T> ref : internalList)
		{
			if(ref != null && ref.get() != null)
				result ^= ref.get().hashCode();
		}			
		
		// Only recalculate this object's hash code when explicitly told to.
		
		// Bad sequence of events:
		// (1) Object is populated with fields that contribute to hashCode
		// (2) Object is placed in a hash table w/r/t hashCode
		// (3) Fields that contributed to hashCode are freed
		// (4) We try to find the key and fail b/c hashCode has changed.
		
		myHashCode = result;
		
		return result;
	}
	
	public T set(int ele, T val)
	{		
		T old;
		
		if(internalList.get(ele) == null)
			old = null;
		else
			old = internalList.get(ele).get();
		
		internalList.set(ele, new WeakReference<T>(val, gced));
		reCalculateHashCode(); 
		
		return old;
	}
	
	public T get(int ele)
	{
		if(internalList.get(ele) == null)
			return null;
		return internalList.get(ele).get();
	}
	
	public int size()
	{
		return internalList.size();
	}
	
	public int hashCode()
	{			
		return myHashCode;
	}
	
	public boolean equals(Object other)
	{
		// (Param must allow for Object, or else the caller will use the method exposed by Object subclass.)
		
		// Check for true equality first. If we don't do this, and contents have been freed, we won't
		// get equals = true.
		if(this == other)			
			return true;		
				
		// element referents are equal (and same size, obviously)
		
		if(! (other instanceof MWeakArrayVector<?>))
			return false;
		MWeakArrayVector<?> otherAsVector = (MWeakArrayVector<?>) other;		
		
		if(otherAsVector.size() != this.size())
			return false;
		
		for(int iIndex=0;iIndex<this.size();iIndex++)
		{
			if(this.get(iIndex) == null && otherAsVector.get(iIndex) == null)
				continue; // both null, ok
			if(this.get(iIndex) == null && otherAsVector.get(iIndex) != null)
				return false;
			if(this.get(iIndex) != null && otherAsVector.get(iIndex) == null)
				return false;
			if( ! this.get(iIndex).equals(otherAsVector.get(iIndex)))
				return false;
		}
		
		return true;
	}
	
	private void cleanUp()
	{
		// Since we are using these for keys, we hope that none of the referents will be
		// reclaimed strictly before the Entry this object appears in is. But play it safe.
		
		// (Actually, is there any need to call cleanUp at all here, since we're fixed size
		//  and get will return null regardless? Seems hanging references are not an issue...)
		
		// not called in other code for now
		
		Reference<? extends T> ref;
		while((ref = gced.poll()) != null)
		{							
			int loc = internalList.indexOf(ref);
			internalList.set(loc, null);						
		}
		
	}
	
	public String toString()
	{
		String result = "[ ";
		
		for(WeakReference<T> ref : internalList)
		{
			if(ref == null)
			{
				result += "null ";
			}
			else
			{
				T val = ref.get();
				
				if(val == null)
					result += "null ";
				else
					result += val.toString() + " ";
				
			}
		}
		
		return result + "]" + " ~ "+myHashCode;
	}
	
}


/*
 * Similar to WeakHashMap, but the _values_ are weak references, not the keys.
 * 
 */

class MWeakValueHashMap<K, V> implements Map<K, V>
{
	// GC will queue the references we are interested in when freeing them	
	private ReferenceQueue<V> gced = new ReferenceQueue<V>();
		
	// Map will handle our mapping nicely. We only wrap it to hide the weak references.
	// Switched to Hashtable from HashMap above; HashMap wasn't working, even when created
	// synchronized. 
	private Map<K, MWeakValue<V>> internalMap = new Hashtable<K, MWeakValue<V>>(); 
	
	// Count of the references reclaimed so far
	long countReclaimed;	
	
	MWeakValueHashMap()
	{
		countReclaimed = 0;
	}
	
	private void cleanUp()
	{		
		Reference<? extends V> ref;
		
		while((ref = gced.poll()) != null)
		{				
			int formerSize = internalMap.size();
								
			MWeakValue<V> weakVal = (MWeakValue)ref;
			
			// It _is_ possible for the poll to return References that are not in the map.
			// In that case...ignore.						
			if(weakVal.getKey() != null)
				internalMap.remove(weakVal.getKey());
			
			if(formerSize > internalMap.size())
				countReclaimed++;
			
			// values().remove seems to do a linear search. Very slow. 
			// Instead, each ref is wrapped along with its key.						
		}
	} 

	public boolean isEmpty()
	{
		cleanUp();
		return internalMap.isEmpty();
	}
	
	public int size()
	{
		cleanUp();
		return internalMap.size();
	}
	
	public V get(Object key)
	{
		cleanUp();
		
		Reference<V> ref = internalMap.get(key);
		if(ref == null)
			return null;
		return ref.get();
	}
		
	public V put(K key, V value) 
	{
		cleanUp();		
		MWeakValue<V> ref = new MWeakValue<V>(key, value, gced);
		Reference<V> oldref = internalMap.put(key, ref);		
		
		if(oldref == null)
			return null;
		return oldref.get();
	}
	
	// Not implemented, because 
	// containsKey? --> get can lead to a race condition with the GC
	// Instead, call get directly.
	/*public boolean containsKey(Object k)
	{
		cleanUp(); 
						
		// Never lie to the caller, saying "Why yes, I have a value for that key" when
		// the value has been deallocated and is in limbo.
		return internalMap.containsKey(k) && (internalMap.get(k).get() != null);
	}*/
	
	public V remove(Object k)
	{
		cleanUp();
		Reference<V> ref = internalMap.remove(k);				
		
		if(ref == null)
			return null;
		
		return ref.get();
	}
		
	public Set<K> keySet()
	{
		cleanUp();
		return internalMap.keySet();
	}
	
	public Set<V> values()
	{
		cleanUp();
		
		// Build list of referents. (These are now HARD REFERENCES!)
		
		Set<V> result = new HashSet<V>();
		for(WeakReference<V> ref : internalMap.values())
		{
			V val = ref.get();
			if(val != null)
				result.add(val);
		}
		
		return result;
	}

	public String toString()
	{
		String result = "[";
		for(K key : internalMap.keySet())
		{
			WeakReference<V> ref = internalMap.get(key); 
			
			if(ref == null)
				result += " " + key.toString() + " -> null";
			else
			{
				V val = ref.get();
				
				 if(val == null)
					result += " " + key.toString() + " -> null";
				else
					result += " " + key.toString() + " ~ hash="+key.hashCode() + " -> " + val.toString();
			}
		}
		
		return result + " ]";
	}

	@Override
	public void clear()
	{
		internalMap.clear();		
	}

	@Override
	public boolean containsKey(Object key) {
		throw new UnsupportedOperationException();	
	}

	@Override
	public boolean containsValue(Object value) {
		throw new UnsupportedOperationException();	
	}

	@Override
	public Set<java.util.Map.Entry<K, V>> entrySet() {
		throw new UnsupportedOperationException();	
	}

	@Override
	public void putAll(Map<? extends K, ? extends V> m) {
		throw new UnsupportedOperationException();	
	}
	  
}


public class MFormulaManager 
{

	
	// **********************************************************

	private static boolean hasBeenInitialized = false;
		
	// **********************************************************
	// Cache maps
	// **********************************************************
	
	// Policies build their IDBs using placeholder Variable objects...
	// ... which are then substituted for variables appearing in the query

	//  -- which doesn't matter at all. We only REALLY need 1 variable object named "x"
	// regardless of whether it first appears as a policy or query variable.
	private static MWeakValueHashMap< String, Variable > vars;
	
	// Only one relation for each name
	private static MWeakValueHashMap< String, Relation> relations;
	
	// Variable tuples: (x, y, z) ... to be tested for membership in Relations
	// Lists and Sets (BUT NOT ARRAYS!)
	// will consider different instances containing the same strings to be "equal":
	private static MWeakValueHashMap< List<String>, Expression> varTuples;

	
	
	// Caches after this point use AST objects as keys, so *keys* must be stored weakly as well:
	
	
	// Atoms: (x, y) in R
	private static MWeakValueHashMap< MWeakArrayVector<Expression>, Formula> atomFormulas;
	
	// Equality: x = y
	private static MWeakValueHashMap< MWeakArrayVector<Variable>, Formula> equalityAtomFormulas; 
	
	// Multiplicity formulas -- singleton!
	private static MWeakValueHashMap< MSetWrapper<Expression>, Formula > loneMultiplicities;
	private static MWeakValueHashMap< MSetWrapper<Expression>, Formula > oneMultiplicities;
	private static MWeakValueHashMap< MSetWrapper<Expression>, Formula > someMultiplicities;
	private static MWeakValueHashMap< MSetWrapper<Expression>, Formula > noMultiplicities;
	
	// Negations: f.not() -- singleton!
	private static MWeakValueHashMap< MSetWrapper<Formula>, Formula> negFormulas;
			
	// Conjunctions: 
	// f1.and(f2)
	// but also Formula.and( set_of_formulas )
	private static MWeakValueHashMap< MSetWrapper<Formula>, Formula> andFormulas;
	
	// Disjunctions
	// f1.or(f2)
	// Formula.or( set_of_formulas )
	private static MWeakValueHashMap< MSetWrapper<Formula>, Formula> orFormulas;	
	
	// Decl: key is always a 2-element list of Variable, Relation.
	private static MWeakValueHashMap< MWeakArrayVector<Expression>, Decl> declNodes;
	
	// Quantifiedformula: key is always a 2-element list of Formula, Decls
	private static MWeakValueHashMap< MWeakArrayVector<Node>, Formula> existsFormulas;
	private static MWeakValueHashMap< MWeakArrayVector<Node>, Formula> forallFormulas;	
	
	/**
	 * Initializes the Formula Manager: all caches are emptied.
	 */
	static void initialize()
	{								
		vars = new MWeakValueHashMap<String, Variable>();
		relations = new MWeakValueHashMap<String, Relation>();
		varTuples = new MWeakValueHashMap< List<String>, Expression>();
		atomFormulas = new MWeakValueHashMap< MWeakArrayVector<Expression>, Formula>();
		negFormulas = new MWeakValueHashMap< MSetWrapper<Formula>, Formula>();
		andFormulas = new MWeakValueHashMap< MSetWrapper<Formula>, Formula>();
		orFormulas = new MWeakValueHashMap< MSetWrapper<Formula>, Formula>();
		oneMultiplicities = new MWeakValueHashMap< MSetWrapper<Expression>, Formula >();
		loneMultiplicities = new MWeakValueHashMap< MSetWrapper<Expression>, Formula >();
		noMultiplicities = new MWeakValueHashMap< MSetWrapper<Expression>, Formula >();
		someMultiplicities = new MWeakValueHashMap< MSetWrapper<Expression>, Formula >();
		equalityAtomFormulas = new MWeakValueHashMap< MWeakArrayVector<Variable>, Formula>();
		
		declNodes = new MWeakValueHashMap<MWeakArrayVector<Expression>, Decl>();
		existsFormulas = new MWeakValueHashMap< MWeakArrayVector<Node>, Formula>();
		forallFormulas = new MWeakValueHashMap< MWeakArrayVector<Node>, Formula>();
		
		hasBeenInitialized = true;			
	}
	
	/**
	 * Prints statistics about the Formula Manager's cache.
	 */
	public static void printStatistics()
	{
		System.out.println(getStatisticsString());
	}
	public static String getStatisticsString()
	{
		if(!hasBeenInitialized)
			initialize();
		
		StringBuffer theResult = new StringBuffer();
				
		theResult.append("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"); theResult.append(MEnvironment.eol);
		theResult.append("Formula Manager statistics:"); theResult.append(MEnvironment.eol);
		theResult.append("Variables: "+vars.size()); theResult.append(MEnvironment.eol);
		theResult.append("Variable Tuples: "+varTuples.size()); theResult.append(MEnvironment.eol);				           
		theResult.append("Relations: "+relations.size()); theResult.append(MEnvironment.eol);
		theResult.append("Decls: "+declNodes.size()); theResult.append(MEnvironment.eol);
		
		int iTotalMultip = someMultiplicities.size() +
		                   noMultiplicities.size() +
		                   loneMultiplicities.size() +
		                   oneMultiplicities.size();
		
		theResult.append("Multiplicity formulas (one, lone, some, and no): "+iTotalMultip); theResult.append(MEnvironment.eol);
		theResult.append("Atomic formulas: "+atomFormulas.size()); theResult.append(MEnvironment.eol);
		theResult.append("Negation formulas: "+negFormulas.size()); theResult.append(MEnvironment.eol);
		theResult.append("Conjunction formulas: "+andFormulas.size()); theResult.append(MEnvironment.eol);
		theResult.append("Disjunction formulas: "+orFormulas.size()); theResult.append(MEnvironment.eol);
		theResult.append("Quantified Formulas (exists): "+existsFormulas.size()); theResult.append(MEnvironment.eol);
		theResult.append("Quantified Formulas (forall): "+forallFormulas.size()); theResult.append(MEnvironment.eol);
		
		int iTotalFormulas = atomFormulas.size() +
		                     negFormulas.size() +
		                     andFormulas.size() +
		                     orFormulas.size() + existsFormulas.size() + forallFormulas.size();
		
		theResult.append("Total formulas: "+ iTotalFormulas); theResult.append(MEnvironment.eol);
		theResult.append("(It is normal for a handful of conjunctions and disjunctions to always remain:"); theResult.append(MEnvironment.eol);
		theResult.append("empty conjunction, empty disjunction, singleton true, etc.)"); theResult.append(MEnvironment.eol);
		theResult.append("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"); theResult.append(MEnvironment.eol);

		MemoryMXBean memoryBean = ManagementFactory.getMemoryMXBean();

		theResult.append("MemoryMXBean statistics (heap):"); theResult.append(MEnvironment.eol);
		theResult.append(memoryBean.getHeapMemoryUsage());		 theResult.append(MEnvironment.eol);

		theResult.append("MemoryMXBean statistics (non-heap):");	 theResult.append(MEnvironment.eol);	
		theResult.append(memoryBean.getNonHeapMemoryUsage()); theResult.append(MEnvironment.eol);
		
		long lReclaimed = 
			someMultiplicities.countReclaimed +
        noMultiplicities.countReclaimed +
        loneMultiplicities.countReclaimed +
        oneMultiplicities.countReclaimed +
        vars.countReclaimed +
        varTuples.countReclaimed +
        relations.countReclaimed +
        atomFormulas.countReclaimed +
        negFormulas.countReclaimed +
        andFormulas.countReclaimed +
        orFormulas.countReclaimed +
        forallFormulas.countReclaimed +
        existsFormulas.countReclaimed;
		
		theResult.append("\nTotal references (Formulas, Variables, Decls, etc.) reclaimed over the life of this Manager: "+lReclaimed); theResult.append(MEnvironment.eol);
                
		
		theResult.append("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"); theResult.append(MEnvironment.eol);
		
		return theResult.toString();
	}
	
	static void ping()
	{
		if(!hasBeenInitialized)
			initialize();
		
		long total =  
			someMultiplicities.size() +
			noMultiplicities.size()+
			loneMultiplicities.size() +
			oneMultiplicities.size() +
			vars.size()+
			varTuples.size()+
			relations.size()+
			atomFormulas.size() +
			negFormulas.size() +
			andFormulas.size() +
			orFormulas.size() +
			declNodes.size() +
			forallFormulas.size() +
			existsFormulas.size();
	}
	
	static Formula makeMultiplicity(Expression e, Multiplicity m)
	throws MGEManagerException
	{
		if(!hasBeenInitialized)
			initialize();
		
		// Use a weak singleton set as key
		final Set<Expression> weakSetKey = Collections.newSetFromMap(
		        new WeakHashMap<Expression, Boolean>());
		weakSetKey.add(e);
		
		// wrap the set to prevent hashCode clobbering by the GC
		final MSetWrapper<Expression> key = new MSetWrapper<Expression>(weakSetKey);
		
		switch(m)
		{
		case LONE:
			Formula cachedValue = loneMultiplicities.get(weakSetKey);
			if(cachedValue != null) 
				return cachedValue; // otherwise, re-create.
			Formula newFormula = e.lone();
			loneMultiplicities.put(key, newFormula);
			return newFormula;
			
		case ONE:
			cachedValue = oneMultiplicities.get(weakSetKey);
			if(cachedValue != null) 
				return cachedValue; // otherwise, re-create.
			newFormula = e.one();
			oneMultiplicities.put(key, newFormula);
			return newFormula;

		case NO:
			cachedValue = noMultiplicities.get(weakSetKey);
			if(cachedValue != null) 
				return cachedValue; // otherwise, re-create.
			newFormula = e.no();
			noMultiplicities.put(key, newFormula);
			return newFormula;


		case SOME:
			cachedValue = someMultiplicities.get(weakSetKey);
			if(cachedValue != null) 
				return cachedValue; // otherwise, re-create.
			newFormula = e.some();
			someMultiplicities.put(key, newFormula);
			return newFormula;

		default:
			throw new MGEManagerException("Unknown Multiplicity: "+m);
		}
	}
	
	
	static Variable makeVariable(String name)
	{
		if(!hasBeenInitialized)
			initialize();
		
		Variable cachedValue = vars.get(name);
		
		if(cachedValue != null)
			return cachedValue; // otherwise, recreate
		
		Variable newVar = Variable.unary(name);
		vars.put(name, newVar);
		return newVar;
	}

	static Relation makeRelation(String name, int arity)
	{
		if(!hasBeenInitialized)
			initialize();		
		
		// We may want two relations named R that have different arity.
		// So we can't just index by relation name: Instead, index by <Name, Arity>
		// (Our :arity suffix isn't reflected in the object at all. It is only used here.)
				
		Relation cachedValue = relations.get(name+":"+arity);		
		if(cachedValue != null)
		{			
			return cachedValue;
		}
		
		final Relation newRel = Relation.nary(name, arity);
		relations.put(name+":"+arity, newRel);
		return newRel;		
	}

	static Expression substituteVarTuple(BinaryExpression expr, HashMap<Variable, Variable> varpairs)
	throws MGEManagerException
	{
		// Given the current variable tuple, replace and rebuild.
		// In-order traversal of tuple tree
		List<String> varnames = MVocab.constructVarNameList(expr); 
		List<String> newvarnames = new ArrayList<String>();
		
		for(String v : varnames)
		{
			Variable vvar = makeVariable(v);
			if(varpairs.containsKey(vvar))
				newvarnames.add(varpairs.get(vvar).name());
			else
				newvarnames.add(v);
		}
		
		return makeVarTuple(newvarnames);		
	}
	
	static Expression makeVarTuple(List<String> varNames)
	throws MGEManagerException
	{
		if(!hasBeenInitialized)
			initialize();		
		
		if(varNames.size() < 1)
			throw new MGEManagerException("makeVarTuple called with empty list.");		
		
		Expression cachedValue = varTuples.get(varNames); 
		if(cachedValue != null)
			return cachedValue;		
		
		Expression newTuple = Expression.NONE; // never used
		boolean first = true;
		for(String name : varNames)
		{
			Variable var = vars.get(name);
			if(var == null)
				var = makeVariable(name);			
			
			if(first)
				newTuple = var;
			else
				newTuple = newTuple.product(var);
			
			first = false;
		}
		
		varTuples.put(varNames, newTuple);
		return newTuple;
	}

	static Expression makeVarTupleV(List<Variable> vars)
	throws MGEManagerException
	{
		// initialization check done in makeVarTuple below.
		List<String> key = new ArrayList<String>(vars.size());
		for(Variable v : vars)
			key.add(v.name());
		
		return makeVarTuple(key);
	}

	static Decl makeOneOfDecl(Variable v, Expression expr)
	throws MGEManagerException
	{		
		if(!hasBeenInitialized)
			initialize();	
		
		// declNodes
		
		if(v == null)
			throw new MGEManagerException("Attempted to make Decl with v parameter null.");			
		if(expr == null)
			throw new MGEManagerException("Attempted to make Decl with expr parameter null.");	
		
		MWeakArrayVector<Expression> key = new MWeakArrayVector<Expression>(2);
		key.set(0, v);
		key.set(1, expr);
				
		Decl cachedValue = declNodes.get(key);		
		if(cachedValue != null)
			return cachedValue;
		
		Decl d = v.oneOf(expr);
		declNodes.put(key, d);
		return d;	
		
	}
	
	static Formula makeForAll(Formula f, Decls d)
	throws MGEManagerException
	{
		if(!hasBeenInitialized)
			initialize();	
		
		// forallFormulas
		
		if(f == null)
			throw new MGEManagerException("Attempted to make quantified formula with f parameter null.");			
		if(d == null)
			throw new MGEManagerException("Attempted to make quantified formula with d parameter null.");			
		
		MWeakArrayVector<Node> key = new MWeakArrayVector<Node>(2);
		key.set(0, f);
		key.set(1, d);
		
		Formula cachedValue = forallFormulas.get(key); 
		if(cachedValue != null)
			return cachedValue;
		
		Formula qf = f.forAll(d);
		forallFormulas.put(key, qf);
		return qf;		
	}
	
	static Formula makeExists(Formula f, Decls d)
	throws MGEManagerException
	{
		if(!hasBeenInitialized)
			initialize();	
		
		// existsFormulas
				
		if(f == null)
			throw new MGEManagerException("Attempted to make quantified formula with f parameter null.");			
		if(d == null)
			throw new MGEManagerException("Attempted to make quantified formula with d parameter null.");			
		
		MWeakArrayVector<Node> key = new MWeakArrayVector<Node>(2);
		key.set(0, f);
		key.set(1, d);
		
		Formula cachedValue = existsFormulas.get(key); 
		if(cachedValue != null)
			return cachedValue;
		
		Formula qf = f.forSome(d);
		existsFormulas.put(key, qf);
		return qf;	
	}

	
	
	static Formula makeAtom(Expression lhs, Expression rhs)
	throws MGEManagerException
	{
		if(!hasBeenInitialized)
			initialize();		
		
		if(lhs == null)
			throw new MGEManagerException("Attempted to make atom with LHS parameter null. RHS was: "+rhs);			
		if(rhs == null)
			throw new MGEManagerException("Attempted to make atom with RHS parameter null. LHS was: "+lhs);			

		
		// Order matters here
		final MWeakArrayVector<Expression> key = new MWeakArrayVector<Expression>(2);
		key.set(0, lhs);
		key.set(1, rhs);
				
		Formula cachedValue = atomFormulas.get(key); 
		if(cachedValue != null)
			return cachedValue;
		
		Formula theFormula = lhs.in(rhs);
		atomFormulas.put(key, theFormula);
		
		return theFormula;			
			
	}
	
	static Formula makeEqAtom(Variable lhv, Variable rhv)
	throws MGEManagerException
	{
		if(!hasBeenInitialized)
			initialize();		
		
		final MWeakArrayVector<Variable> key = new MWeakArrayVector<Variable>(2);
		key.set(0, lhv);
		key.set(1, rhv);	
		
		Formula cachedValue = equalityAtomFormulas.get(key); 
		if(cachedValue != null)
			return cachedValue;
		
		Formula theFormula = lhv.eq(rhv);
		equalityAtomFormulas.put(key, theFormula);
		return theFormula;			
			
	}
	
	static Formula makeNegation(Formula f)
	{
		// ORIGINAL KEY
		
		// Use a weak singleton set as key
		Set<Formula> weakSetKey = Collections.newSetFromMap(
		        new WeakHashMap<Formula, Boolean>());
		weakSetKey.add(f);
		// and wrap it, to prevent hashcode clobbering by the GC
		MSetWrapper<Formula> key = new MSetWrapper<Formula>(weakSetKey);
		
		// -----------------------------------
		
		Formula cachedValue = negFormulas.get(key); 
		if(cachedValue != null)
			return cachedValue;

		// -----------------------------------
		
		Formula neg;
		
		// No reason to create a double-negation, 
		// or ever negate a constant true/false						
		if(f instanceof NotFormula)
		{
			neg = ((NotFormula)f).formula();
			negFormulas.put(key, neg);
			return neg;
		}
		if(Formula.TRUE.equals(f))
		{			
			neg = Formula.FALSE;
			negFormulas.put(key, neg);
			return neg;
		}
		if(Formula.FALSE.equals(f))
		{
			neg = Formula.TRUE;
			negFormulas.put(key, neg);
			return neg;
		}
		
		// The functions are being called properly; no reason to ever
		// receive an exception
		try
		{
		
			if(f instanceof BinaryFormula)
			{	
				neg = internalNegBinary((BinaryFormula)f);
			}
			else if(f instanceof NaryFormula)
			{
				neg = internalNegNary((NaryFormula)f);
			}
			else if (f instanceof QuantifiedFormula)
			{
				neg = internalNegQuant((QuantifiedFormula)f);
			}
			else if(f instanceof ComparisonFormula)
			{
				neg = f.not();
			}		
			else if(f instanceof MultiplicityFormula)
			{
				// treat as atom for now
				neg = f.not();
			}
			else
			{
				neg = null;
				System.err.println("Unsupported Formula type: "+f.getClass());
				System.exit(1);
			}
			
			
			// -----------------------------------
			// No reason to worry about a 2nd key here;
			//  -- constant formulas are unique
			//  -- comparison formulas are unique (up to being created by this object)
			//  -- the other funcs invoke funcs from this object
					
			// -----------------------------------
			
			
			negFormulas.put(key, neg);
			return neg;
			
		}
		catch(MGEManagerException e)
		{
			System.err.println("Unexpected Manager exception in makeNegation: "+e);
			System.exit(1);
		}

		return null;
	}
	
	private static Formula internalNegNary(NaryFormula within)
	throws MGEManagerException 
	{		
		
		Set<Formula> newformulas = new HashSet<Formula>();
		for (Formula fmla : within)
		{
			Formula negfmla = makeNegation(fmla);
			newformulas.add(negfmla);
		}

		// De Morgan's law:
		switch (within.op())
		{
		case AND:
			return MFormulaManager.makeDisjunction(newformulas);
		case OR:
			return MFormulaManager.makeConjunction(newformulas);
		default:
			System.err.println("Warning: NaryFormula operator "
					+ within.op() + " not supported.");
			System.exit(1);
		}
		return null;
	}

	private static Formula internalNegQuant(QuantifiedFormula f) 
	throws MGEManagerException
	{
		// Either way, we negate the internal fmla
		Formula inner = makeNegation(f.formula());
	
		if(f.quantifier().equals(Quantifier.ALL))
		{
			return makeExists(inner, f.decls());
		}
		else
		{
			return makeForAll(inner, f.decls());
		}
	}

	private static Formula internalNegBinary(BinaryFormula within)
	throws MGEManagerException 
	{
		Formula newleft;
		Formula newright;
		
		switch (within.op())
		{
		// Normal cases. Apply De Morgan's and revisit.
		case AND:
			newleft = makeNegation(within.left());
			newright = makeNegation(within.right());
			return MFormulaManager.makeOr(newleft, newright);

		case OR:
			newleft = makeNegation(within.left());
			newright = makeNegation(within.right());
			return MFormulaManager.makeAnd(newleft, newright);

			// a and !b
		case IMPLIES:
			newleft = within.left();
			newright = makeNegation(within.right());
			return MFormulaManager.makeAnd(newleft, newright);

			// (a and !b) or (b and !a)
		case IFF:
			newleft = MFormulaManager.makeAnd(within.left(),
					makeNegation(within.right()));
			newright = MFormulaManager.makeAnd(within.right(),
					makeNegation(within.left()));
					
			return MFormulaManager.makeOr(newleft, newright);

		}
		return null;
	}

	static Formula makeConjunction(Set<Formula> conjuncts)
	{
		
		// Use weak keys 
		Set<Formula> weakSetKey = Collections.newSetFromMap(
		        new WeakHashMap<Formula, Boolean>());
		weakSetKey.addAll(conjuncts);
		// and wrap it, to prevent hashcode clobbering by the GC
		MSetWrapper<Formula> key = new MSetWrapper<Formula>(weakSetKey);

		// -----------------------------------
		
		Formula cachedValue = andFormulas.get(key); 
		if(cachedValue != null)
			return cachedValue;

		// -----------------------------------
		// Pull up matching children (e.g. and(and(x, y), z) should be and(x, y, z))
		
		Set<Formula> newConjuncts = new HashSet<Formula>();
		for(Formula f : conjuncts)
		{
			if(f instanceof BinaryFormula && ((BinaryFormula)f).op().equals(FormulaOperator.AND))
			{
				newConjuncts.add(((BinaryFormula)f).left());
				newConjuncts.add(((BinaryFormula)f).right());				
			}
			else if(f instanceof NaryFormula && ((NaryFormula)f).op().equals(FormulaOperator.AND))
			{
				for(Formula childf : ((NaryFormula)f))
				{
					newConjuncts.add(childf);
				}
			}
			else
				newConjuncts.add(f);
		}
		conjuncts = newConjuncts;
		
		// -----------------------------------
		
		// Find trivialities
		Set<Formula> toRemove = new HashSet<Formula>();		
		for(Formula f : conjuncts)
		{
			if(Formula.TRUE.equals(f))
				toRemove.add(f);
			if(Formula.FALSE.equals(f))
			{
				// impossible
				andFormulas.put(key, Formula.FALSE);
				return Formula.FALSE;
			}				
		}
		
		// Remove trivialities
		Set<Formula> toConj;
		MSetWrapper<Formula> key2;
		if(toRemove.size() > 0)
		{
			toConj = new HashSet<Formula>(conjuncts);
			toConj.removeAll(toRemove);
						
			Set<Formula> weakSetKey2 = Collections.newSetFromMap(
			        new WeakHashMap<Formula, Boolean>());			
			weakSetKey2.addAll(toConj);			
			key2 = new MSetWrapper<Formula>(weakSetKey2);			
		}
		else
		{
			toConj = conjuncts;
			key2 = null;
		}
		
		
		// -----------------------------------
		// Start key saving / return section
		
		
		// If we've already created this normal form...
		if(key2 != null)
		{
			cachedValue = andFormulas.get(key2); 
			if(cachedValue != null)
			{
				andFormulas.put(key, cachedValue); // save for orig. key
				return cachedValue;
			}
		}
				
		// -----------------------------------

		// Create the new formula via Kodkod's API
		final Formula conjunction = Formula.and(toConj); // don't use weak set in constructor
		
		// -----------------------------------
		
		// Save for original key
		andFormulas.put(key, conjunction);
		
		// Did we alter the formula before creating it? Then
		// we should also save for THAT new key:
		if(key2 != null)
		{
			andFormulas.put(key2, conjunction);
		}		
		
		// -----------------------------------
		
		return conjunction;		
		
		// TODO
		// promotion of existentials when safe; substitution for it
	}

	static Formula makeDisjunction(Set<Formula> disjuncts)
	{
		// Will automatically use BinaryFormula if applicable.
				
		// Use weak keys
		Set<Formula> weakSetKey = Collections.newSetFromMap(
		        new WeakHashMap<Formula, Boolean>());
		weakSetKey.addAll(disjuncts);
		// and wrap it, to prevent hashcode clobbering by the GC
		MSetWrapper<Formula> key = new MSetWrapper<Formula>(weakSetKey);
			
		// -----------------------------------
		
		Formula cachedValue = orFormulas.get(key); 
		if(cachedValue != null)
			return cachedValue;	
		
		// -----------------------------------
		// Pull up matching children (e.g. and(and(x, y), z) should be and(x, y, z))
		
		Set<Formula> newDisjuncts = new HashSet<Formula>();
		for(Formula f : disjuncts)
		{
			if(f instanceof BinaryFormula && ((BinaryFormula)f).op().equals(FormulaOperator.OR))
			{
				newDisjuncts.add(((BinaryFormula)f).left());
				newDisjuncts.add(((BinaryFormula)f).right());				
			}
			else if(f instanceof NaryFormula && ((NaryFormula)f).op().equals(FormulaOperator.OR))
			{
				for(Formula childf : ((NaryFormula)f))
				{
					newDisjuncts.add(childf);
				}
			}
			else
				newDisjuncts.add(f);
		}
		disjuncts = newDisjuncts;
		
		
		
		// -----------------------------------
		
		// Find trivialities
		Set<Formula> toRemove = new HashSet<Formula>();		
		for(Formula f : disjuncts)
		{
			if(Formula.FALSE.equals(f))
				toRemove.add(f);
			if(Formula.TRUE.equals(f))
			{
				// tautology
				orFormulas.put(key, Formula.TRUE);
				return Formula.TRUE;
			}				
		}
		
		// Remove trivialities
		Set<Formula> toDisj;
		MSetWrapper<Formula> key2;
		if(toRemove.size() > 0)
		{
			toDisj = new HashSet<Formula>(disjuncts);
			toDisj.removeAll(toRemove);
						
			Set<Formula> weakSetKey2 = Collections.newSetFromMap(
			        new WeakHashMap<Formula, Boolean>());			
			weakSetKey2.addAll(toDisj);			
			key2 = new MSetWrapper<Formula>(weakSetKey2);			
		}
		else
		{
			toDisj = disjuncts;
			key2 = null;
		}
		
		
		// -----------------------------------
		// Start key saving / return section
		
		
		// If we've already created this normal form...
		if(key2 != null)
		{
			cachedValue = orFormulas.get(key2); 
			if(cachedValue != null)
			{
				orFormulas.put(key, cachedValue); // save for orig. key
				return cachedValue;
			}
		}
				
		// -----------------------------------

		// Create the new formula via Kodkod's API
		
		final Formula disjunction = Formula.or(disjuncts); // don't use weak set in constructor
		
		// Save for original key
		orFormulas.put(key, disjunction);
		
		// Did we alter the formula before creating it? Then
		// we should also save for THAT new key:
		if(key2 != null)
		{
			orFormulas.put(key2, disjunction);
		}	
		
		// -----------------------------------
		
		return disjunction;
	}
	
	static Formula makeAnd(Formula f1, Formula f2)
	{
		Set<Formula> conjuncts = new HashSet<Formula>();
		conjuncts.add(f1);
		conjuncts.add(f2);
		
		return makeConjunction(conjuncts);
	}
	
	static Formula makeOr(Formula f1, Formula f2)
	{
		Set<Formula> disjuncts = new HashSet<Formula>();
		disjuncts.add(f1);
		disjuncts.add(f2);
		
		return makeDisjunction(disjuncts);
	}

	static Formula makeComposition(FormulaOperator op, Set<Formula> formulas) throws MGEManagerException
	{
		if(FormulaOperator.OR.equals(op))
			return makeDisjunction(formulas);
		if(FormulaOperator.AND.equals(op))
			return makeConjunction(formulas);
		throw new MGEManagerException("makeComposition (set): Non OR/AND operators not supported: "+op);
	}
	
	static Formula makeComposition(FormulaOperator op, Formula left, Formula right) throws MGEManagerException
	{
		if(FormulaOperator.OR.equals(op))
			return makeOr(left, right);
		if(FormulaOperator.AND.equals(op))
			return makeAnd(left, right);
		if(FormulaOperator.IMPLIES.equals(op))
			return makeOr( makeNegation(left), right);
		throw new MGEManagerException("makeComposition (binary): Non OR/AND/IMPLIES operators not supported: "+op);
	}
	
	static Formula makeImplication(Formula ante, Formula cons) 
	{
		// a -> b is equivalent to !a or b
		Formula negAnte = makeNegation(ante);
		return makeOr(negAnte, cons);
	}
	
	static Formula makeIFF(Formula a, Formula b)
	{
		// a <-> b is equivalent to (a and b) or (!a and !b)
		Formula negA = makeNegation(a);
		Formula negB = makeNegation(b);		
		return makeOr(makeAnd(a, b), makeAnd(negA, negB));
	}
	
	static void printAtoms(String substr)
	{
		System.out.println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
		if(substr.length() < 1)
			System.out.println("All atomic formulas known to the manager:");
		else
			System.out.println("All atomic formulas known to the manager (containing the substring "+substr+"):");
		for(Formula f : atomFormulas.values())
			if(substr.length() < 1 || f.toString().contains(substr))
			System.out.println(f + ": "+f.hashCode());
		System.out.println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
	}

	static void clearAll()
	{
		// Wipe all caches
		// Use with care: This will NOT do anything to existing Formulas that are reachable.
		// Not public for this reason.
		
		vars.clear();
		relations.clear();
		varTuples.clear();
		atomFormulas.clear();
		negFormulas.clear();
		andFormulas.clear();
		orFormulas.clear();
		oneMultiplicities.clear();
		loneMultiplicities.clear();
		noMultiplicities.clear();
		someMultiplicities.clear();
		equalityAtomFormulas.clear();
		
		declNodes.clear();
		existsFormulas.clear();
		forallFormulas.clear();
		
		
	}		
	
	static boolean varIsSaved(String varname)
	{
		// Can't use containsKey. It isn't trustworthy when using
		// WeakReferences because of race conditions.
		
		Variable var = vars.get(varname);
		//System.out.println("Searching for "+varname+" got "+var);
		return var != null;		
	}

	public static Element getStatisticsNode(Document xmldoc)
	{
		// Return a structured object with statistics, instead of a string.
		if(!hasBeenInitialized)
			initialize();
			
		Element theResult = xmldoc.createElementNS(null, "MANAGER");
			
		theResult.setAttribute("num-variables", String.valueOf(vars.size()));
		theResult.setAttribute("variable-tuples", String.valueOf(varTuples.size()));
		theResult.setAttribute("relations", String.valueOf(relations.size()));
		theResult.setAttribute("decls", String.valueOf(declNodes.size()));
										          			
		int iTotalMultip = someMultiplicities.size() +
		                   noMultiplicities.size() +
		                   loneMultiplicities.size() +
		                   oneMultiplicities.size();
		
		theResult.setAttribute("multiplicity", String.valueOf(iTotalMultip));
		theResult.setAttribute("atoms", String.valueOf(atomFormulas.size()));
		theResult.setAttribute("negations", String.valueOf(negFormulas.size()));
		theResult.setAttribute("conjunctions", String.valueOf(andFormulas.size()));
		theResult.setAttribute("disjunctions", String.valueOf(orFormulas.size()));
		theResult.setAttribute("q-exists", String.valueOf(existsFormulas.size()));
		theResult.setAttribute("q-forall", String.valueOf(forallFormulas.size()));
									
		int iTotalFormulas = atomFormulas.size() +
		                     negFormulas.size() +
		                     andFormulas.size() +
		                     orFormulas.size() + existsFormulas.size() + forallFormulas.size();

		theResult.setAttribute("total-formulas", String.valueOf(iTotalFormulas));
	
		MemoryMXBean memoryBean = ManagementFactory.getMemoryMXBean();

		Element thisElement;
		thisElement = xmldoc.createElementNS(null, "HEAP-USAGE");
		thisElement.setAttribute("units", "bytes");		
		MemoryUsage mu = memoryBean.getHeapMemoryUsage();
		thisElement.setAttribute("init", String.valueOf(mu.getInit()));
		thisElement.setAttribute("used", String.valueOf(mu.getUsed()));
		thisElement.setAttribute("max", String.valueOf(mu.getMax()));
		theResult.appendChild(thisElement);		

		
		thisElement = xmldoc.createElementNS(null, "NON-HEAP-USAGE");
		thisElement.setAttribute("units", "bytes");		
		mu = memoryBean.getNonHeapMemoryUsage();
		thisElement.setAttribute("init", String.valueOf(mu.getInit()));
		thisElement.setAttribute("used", String.valueOf(mu.getUsed()));
		thisElement.setAttribute("max", String.valueOf(mu.getMax()));
		theResult.appendChild(thisElement);			

		
		long lReclaimed = 
			someMultiplicities.countReclaimed +
	       noMultiplicities.countReclaimed +
	       loneMultiplicities.countReclaimed +
	       oneMultiplicities.countReclaimed +
	       vars.countReclaimed +
	       varTuples.countReclaimed +
	       relations.countReclaimed +
	       atomFormulas.countReclaimed +
	       negFormulas.countReclaimed +
	       andFormulas.countReclaimed +
	       orFormulas.countReclaimed +
	       forallFormulas.countReclaimed +
	       existsFormulas.countReclaimed;
			
		theResult.setAttribute("total-reclaimed", String.valueOf(lReclaimed));		
			
		return theResult;		
	}
}



