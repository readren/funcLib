package readren.funcLib.propCheck

import readren.funcLib.util.Rng
import readren.funcLib.dataTypes.Gen
import scala.annotation.tailrec
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import readren.funcLib.dataTypes.SGen

case class Prop(run: (MaxSize, TestCases, Rng) => Result) {
	//Ejercicio 9a
	def &&(p: Prop): Prop =
		Prop { (maxSize, testCases, rng) =>
			run(maxSize, testCases, rng) match {
				case Passed => p.run(maxSize, testCases, rng)
				case f: Falsified => f
			}
		}

	//Ejercicio 9b
	def ||(p: Prop): Prop =
		Prop { (maxSize, testCases, rng) =>
			run(maxSize, testCases, rng) match {
				case Passed => Passed
				case f: Falsified => p.run(maxSize, testCases, rng)
			}
		}
}

object Prop {
	// Versión echa por mi antes de ver la desarrollada en el libro. Me faltó el "Try" que lo agregué luego de ver la versión del libro.
	def forAll[A](ga: Gen[A])(f: A => Boolean): Prop =
		Prop { (maxSize, testCases, rng0) =>
			@tailrec
			def loop(remainingCases: TestCases, rng1: Rng): Result = {
				if (remainingCases == 0) Passed
				else {
					val (a, rng2) = ga(rng1)
					Try(f(a)) match {
						case Failure(e) =>
							Falsified(buildMsg(a, e), testCases - remainingCases)
						case Success(b) =>
							if (b) loop(remainingCases - 1, rng2)
							else Falsified(a.toString(), testCases - remainingCases)
					}
				}
			}
			loop(testCases, rng0)
		}


	// copiado del libro
	def forAllProgresively[A](g: SGen[A])(f: A => Boolean): Prop = Prop {
		(max, n, rng) =>
			val casesPerSize = (n + (max - 1)) / max
			val props: Stream[Prop] =
				Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
			val prop: Prop =
				props.map(p => Prop { (max, _, rng) =>
					p.run(max, casesPerSize, rng)
				}).toList.reduce(_ && _)
			prop.run(max, n, rng)
	}

	private[propCheck] def buildMsg[A](s: A, e: Throwable): String =
		s"test case: $s\n" +
			s"generated an exception: ${e.getMessage}\n" +
			s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

	def run(p: Prop,
		maxSize: Int = 10,
		testCases: Int = 100,
		rng: Rng = Rng.simple(System.currentTimeMillis)): Unit =
		p.run(maxSize, testCases, rng) match {
			case Falsified(msg, n) =>
				println(s"! Falsified after $n passed tests:\n $msg")
			case Passed =>
				println(s"+ OK, passed $testCases tests.")
		}
}