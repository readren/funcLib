package readren.funcLib

package object propCheck {
	type TestCases = Int
	type FailedCase = String
	type SuccessCount = Int
	type MaxSize = Int

	sealed trait Result {
		def isFalsified: Boolean
	}
	case object Passed extends Result {
		def isFalsified = false
	}
	case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
		def isFalsified = true
	}

}