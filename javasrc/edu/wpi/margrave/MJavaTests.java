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
		MEnvironment.writeOutLine("Starting java-based tests.");
		
		/////////////////////////////////////////////////////////////
		RelationAndVariableReplacementV.unitTests();
		/////////////////////////////////////////////////////////////
		MQuery.unitTest();
		/////////////////////////////////////////////////////////////
		MVocab.unitTest();
		
		MPolicyLeaf.unitTests();
		MPolicySet.unitTests();
		
/////////////////////////////////////////////////////////////
		try {
			FormulaSigInfo.unitTests();
		} catch (MNotASortException e) {
			
			MEnvironment.writeOutLine(">>> FormulaSigInfo tests FAILED: exception thrown!\n"+e.getLocalizedMessage());
		}
		
		
		
		// Do not put anything after this point except the call to exitJavaTests.
		/////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////
		exitJavaTests(1);
	
		// do_time_tupling_new();
		// 		exitJavaTests(1);

		// do_time_tupling();
		// 		exitJavaTests(1);

	//	benchmarkXACML();
	//			exitJavaTests(1);
						
/*
		
		
				
		
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
	

}

