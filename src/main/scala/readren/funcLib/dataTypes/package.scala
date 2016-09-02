package readren.funcLib

import readren.funcLib.util.Rng

package object dataTypes {
	type Gen[+A] = StateAlgebra[Rng]#Transition[A]
	type SGen[+A] = Int => Gen[A]
}