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

// tn

package edu.wpi.margrave;


public class MJavaTests
{

	
	public static void exitJavaTests(int arg)
	{
		MEnvironment.flushBuffers("Java Tests");
		System.exit(arg);
	}

	
	public static void main(String[] args)
	throws MBaseException
	{

	   /* Variable v = Variable.unary("v");
		Relation r = Relation.unary("r");

		WeakReference ref1 = new WeakReference<Variable>(v);
		WeakReference ref2 = new WeakReference<Variable>(v);

		MEnvironment.writeErrLine(ref1.equals(ref2));

		// ok, so references aren't equal based on referents.
		String x = "foo";
		ref1 = new WeakReference(x);
		ref2 = new WeakReference(x);

		MEnvironment.writeErrLine(ref1.equals(ref2));

		 Set<Formula> weakHashSet1 = Collections.newSetFromMap(
			        new WeakHashMap<Formula, Boolean>());

		 Set<Formula> weakHashSet2 = Collections.newSetFromMap(
			        new WeakHashMap<Formula, Boolean>());

		 Formula f = v.in(r);

			MEnvironment.writeErrLine(weakHashSet1.size());
		weakHashSet1.add(f);
		weakHashSet2.add(f);

		MEnvironment.writeErrLine(weakHashSet1.equals(weakHashSet2));

		Set<Formula> strongSet = new HashSet<Formula>();
		strongSet.add(f);
		MEnvironment.writeErrLine();
		MEnvironment.writeErrLine(strongSet.equals(weakHashSet1));
		MEnvironment.writeErrLine(weakHashSet1.equals(strongSet));
		MEnvironment.writeErrLine(weakHashSet1.size());

		MEnvironment.writeErrLine(weakHashSet1);
		f = Formula.TRUE;

		System.gc();

		// cannot trust size()? but contains and empty are trustworthy
		MEnvironment.writeErrLine(weakHashSet1.size());
		MEnvironment.writeErrLine(weakHashSet1);
		MEnvironment.writeErrLine(weakHashSet1.size());
		MEnvironment.writeErrLine(weakHashSet1.contains(f));
		MEnvironment.writeErrLine(weakHashSet1.isEmpty());



		// Test common sub-exp removal

		Formula f1 = v.in(r);
		Formula f2 = v.in(r);

		BinaryFormula conj = (BinaryFormula) f1.and(f2);

		MEnvironment.writeErrLine(conj.left().hashCode());
		MEnvironment.writeErrLine(conj.right().hashCode());




		// cool idea: canonical weak ref in an object itself... but can't do that for Formula. (Without wrapper, anyway) Pity...

		HashMap<Formula, Integer> hm1 = new HashMap<Formula, Integer>();
		HashMap<Formula, Integer> hm2 = new HashMap<Formula, Integer>();

		MEnvironment.writeErrLine(hm1.equals(hm2));

		hm1.put(f2, Integer.valueOf(2));
		hm1.put(f, Integer.valueOf(1));

		MEnvironment.writeErrLine(hm1.equals(hm2));

		hm2.put(f, Integer.valueOf(1));
		hm2.put(f2, Integer.valueOf(2));
		MEnvironment.writeErrLine(hm1.equals(hm2));

 */

		/*MWeakArrayVector<Variable> arr1 = new MWeakArrayVector<Variable>(2);
		MWeakArrayVector<Variable> arr2 = new MWeakArrayVector<Variable>(2);

		Variable v1 = Variable.unary("v1");
		Variable v2 = Variable.unary("v2");
		Variable v3 = Variable.unary("v3");
		Variable v4 = Variable.unary("v4");
		Variable v5 = Variable.unary("v5");

		arr1.set(0, v1);
		arr2.set(0, v1);

		MEnvironment.writeErrLine(arr1.equals(arr2));
		MEnvironment.writeErrLine(arr1.hashCode() + " " + arr2.hashCode());

		arr1.set(1, v2);

		MEnvironment.writeErrLine(arr1.equals(arr2));
		MEnvironment.writeErrLine(arr1.hashCode() + " " + arr2.hashCode());

		arr2.set(1, v2);

		MEnvironment.writeErrLine(arr1.equals(arr2));
		MEnvironment.writeErrLine(arr1.hashCode() + " " + arr2.hashCode());

		*/

		// do_time_tupling_new();
		// 		exitJavaTests(1);

		// do_time_tupling();
		// 		exitJavaTests(1);

	//	benchmarkXACML();
	//			exitJavaTests(1);
						
/*
		MEnvironment.writeOutLine("Starting java-based tests.");
		
		RelationAndVariableReplacementV.unitTests();
		MQuery.unitTest();
		MVocab.unitTest();
	
		try {
			FormulaSigInfo.unitTests();
		} catch (MNotASortException e) {
			
			MEnvironment.writeOutLine(">>> FormulaSigInfo tests FAILED: exception thrown!\n"+e.getLocalizedMessage());
		}
		
		
		// Main test blocks		
		runTests();		
		
		// 		exitJavaTexts(1);

		
		// tupling benchmarks
		MEnvironment.writeOutLine("\n\nTUPLING BENCHMARKS\n\n");
		// Not _new; the new timer uses unary preds for address bits
		do_time_tupling();					
				

	
		
		
		// Everything from tests should be out of scope now.
		// Test that the weak references in the manager are doing their job.
		//System.gc();
		//MEnvironment.writeOutLine("State of Formula Manager after forced gc call: ");
		//MFormulaManager.printStatistics();

		//System.gc();
		//MFormulaManager.ping();
		//System.gc();
		//MFormulaManager.printStatistics();

*/
		/*
		 * Leave everything after the above printStatistics() call empty.
		 */
	}
	
	/*
	public static void benchmarkXACML(String continueFileName)
    throws MUserException
    {
		
        ThreadMXBean mxBean = ManagementFactory.getThreadMXBean();
        long start = mxBean.getCurrentThreadCpuTime();

        int numTrials = 100;

        MEnvironment.writeErrLine("Beginning XACML benchmark.");

        // Load the CONTINUE policy. Do it n times.
        for(int ii = 0; ii<numTrials; ii++)
        {
            MPolicy.readXACML10(continueFileName);

           // MEnvironment.writeErrLine(foo.idbs.keySet().size());
            //MEnvironment.writeErrLine(foo.idbs.keySet());

            // First test, make sure that data structures aren't being reused and saving us a bunch of time
            // on all but first iteration:
            //if(ii % 10 == 0)
            	MEnvironment.writeErrLine( ((mxBean.getCurrentThreadCpuTime()-start)/1000000) / (ii+1));

            MFormulaManager.clearAll();
            System.gc();
        }

        MEnvironment.writeErrLine("Average time to load policy: "+((mxBean.getCurrentThreadCpuTime()-start)/1000000)/numTrials+
        		"ms. Number of trials was: "+numTrials);

        // Tests on the policy
        MPolicy continuePolicy = MPolicy.readXACML10(continueFileName);

       /start = mxBean.getCurrentThreadCpuTime();
        for(int ii = 0; ii<numTrials; ii++)
        {
        	MQuery q = continuePolicy.queryPolicy("(forsome s Subject (forsome a Action (forsome r Resource (forsome e Environment (RPSlist:Permit s a r e)))))");
        	q.isQuerySatisfiable();
        	MEnvironment.writeErrLine( ((mxBean.getCurrentThreadCpuTime()-start)/1000000) / (ii+1));

        }

        MEnvironment.writeErrLine("Average time to run basic permit query: "+((mxBean.getCurrentThreadCpuTime()-start)/1000000)/numTrials+
        		"ms. Number of trials was: "+numTrials);



        start = mxBean.getCurrentThreadCpuTime();
        for(int ii = 0; ii<numTrials; ii++)
        {
        	MQuery q = continuePolicy.queryPolicy("(forsome s Subject (forsome a Action (forsome r Resource (forsome e Environment (RPSlist:Permit s a r e)))))");
        	q.doTupling = true;
        	q.isQuerySatisfiable();
        	MEnvironment.writeErrLine( ((mxBean.getCurrentThreadCpuTime()-start)/1000000) / (ii+1));

        }

        MEnvironment.writeErrLine("Average time to run basic permit query WITH TUPLING: "+((mxBean.getCurrentThreadCpuTime()-start)/1000000)/numTrials+
        		"ms. Number of trials was: "+numTrials);



    }*/

}

