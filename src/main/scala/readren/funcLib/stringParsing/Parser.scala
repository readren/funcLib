package readren.funcLib.stringParsing

import scala.language.implicitConversions;
import scala.Left
import scala.Right
import scala.util.matching.Regex;

case class Match[+A](value: A, nextOffset: Int)

case class Error(parseError: ParseError, commited: Boolean)

abstract class Parser[+A] {
	def apply(input: String, offset: Int = 0): Either[Error, Match[A]]
}

object Parser extends ParserAlgebra[Parser] {
	implicit def apply[A](parse: (String, Int) => Either[Error, Match[A]]) = new Parser[A] {
		def apply(input: String, offset: Int) = parse(input, offset)
	}

	private def buildError(input: String, offset: Int, commited: Boolean, message: String) =
		Left(Error(ParseError(List(Layer(offset, message, Nil))), commited))

	def run[A](p: Parser[A])(input: String, offset: Int = 0): Either[ParseError, A] = {
		p(input, offset).fold(x => Left(x.parseError), x => Right(x.value))
	}

	def succeed[A](a: A): Parser[A] = {
		(input: String, offset: Int) =>
			Right(Match(a, offset))
	};
	def unit[A](a: A): Parser[A] = succeed(a);
	def lazyUnit[A](a: => A): Parser[A] =
		(input: String, offset: Int) =>
			Right(Match(a, offset))

	implicit def string(s: String): Parser[String] = {
		(input: String, offset: Int) =>
			if (input.startsWith(s, offset)) Right(Match(s, offset + s.length))
			else buildError(input, offset, false, "'" + s + "' was expected, but '" + input.substring(offset, math.min(input.length, offset + s.length)) + "' was found")
	}

	implicit def regex(r: Regex): Parser[String] = {
		(input: String, offset: Int) =>
			r.findFirstMatchIn(input.substring(offset)) match {
				case Some(m) => Right(Match(m.matched, offset + m.end))
				case None => buildError(input, offset, false, "A segment that matches '" + r + "' was expected, but '" + input.substring(offset, math.min(input.length, offset + r.regex.length)) + "' was found")
			}
	}

	def slice[A](pa: Parser[A]): Parser[String] = {
		(input: String, offset: Int) =>
			pa(input, offset).right.map { r => Match(input.substring(offset, r.nextOffset), r.nextOffset) }
	}

	def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B] = {
		(input: String, offset: Int) =>
			pa(input, offset) match {
				case Left(pe) => Left(pe)
				case Right(result) =>
					val pb = f(result.value)
					pb(input, result.nextOffset).left.map(x => Error(x.parseError, true))
			}
	}

	def label[A](text: String)(pa: Parser[A]): Parser[A] = {
		(input: String, offset: Int) =>
			pa(input, offset).left.map { e =>
				Error(e.parseError.updateTopMessage(text), e.commited)
			}
	}

	def scope[A](text: String)(pa: Parser[A]): Parser[A] = {
		(input: String, offset: Int) =>
			pa(input, offset).left.map { e =>
				Error(e.parseError.push(offset, text), e.commited)
			}
	}

	def or[A](pa: Parser[A], pb: => Parser[A]): Parser[A] = {
		(input: String, offset: Int) =>
			pa(input, offset) match {
				case Left(Error(parseError1, false)) =>
					pb(input, offset).left.map {
						case Error(parseError2, commited2) =>
							Error(parseError1.updateTopBranch(parseError2), commited2)
					}
				case r => r
			}

	}

	def attempt[A](pa: Parser[A]): Parser[A] = {
		(input: String, offset: Int) =>
			pa(input, offset).left.map { e => Error(e.parseError, false) }
	}

}
