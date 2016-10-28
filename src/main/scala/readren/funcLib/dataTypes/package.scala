package readren.funcLib

import readren.funcLib.util.Rng

package object dataTypes {
	/**Alias of the data type over which the algebra implemented by the `Gen` object operates. The `Gen` object is an implementation of the `GenAlgebra`.
	 * Originally the name of this alias was `Gen`, but that name causes trouble to scala-ide (false compilation errors). */
	type GenDt[+A] = StateAlgebra[Rng]#Transition[A];
	
	/**Alias of the data type over which the algebra implemented by the `SGen` object operates. The `SGen` object is an implementation of the `SGenAlgebra` .
	 * Originally the name of this alias was `SGen`, but that name causes trouble to scala-ide (false compilation errors). */
	type SGenDt[+A] = Int => GenDt[A]
}