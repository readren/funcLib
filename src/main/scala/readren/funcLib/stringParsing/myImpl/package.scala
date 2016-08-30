package readren.funcLib.stringParsing

package object myImpl {

	case class Match[+A](value: A, nextOffset: Int)

	case class Error(parseError: ParseError, commited: Boolean)

	abstract class MyParser[+A] {
		def apply(input: String, offset: Int = 0): Either[Error, Match[A]]
	}

	object MyParser {
		implicit def apply[A](parse: (String, Int) => Either[Error, Match[A]]) = new MyParser[A] {
			def apply(input: String, offset: Int) = parse(input, offset)
		}
	}
}