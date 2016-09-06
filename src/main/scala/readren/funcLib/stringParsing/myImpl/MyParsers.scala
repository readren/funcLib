package readren.funcLib.stringParsing.myImpl

import scala.language.implicitConversions;
import scala.Left
import scala.Right
import scala.util.matching.Regex;

import readren.funcLib.stringParsing.Layer
import readren.funcLib.stringParsing.ParseError
import readren.funcLib.stringParsing.ParserAlgebra

object MyParsers extends ParserAlgebra[MyParser] {

	private def buildError(input: String, offset: Int, commited: Boolean, message: String) =
		Left(Error(ParseError(List(Layer(offset, message, Nil))), commited))

	def run[A](p: MyParser[A])(input: String, offset: Int = 0): Either[ParseError, A] = {
		p(input, offset).fold(x => Left(x.parseError), x => Right(x.value))
	}

	def succeed[A](a: A): MyParser[A] = {
		(input: String, offset: Int) =>
			Right(Match(a, offset))
	}

	implicit def string(s: String): MyParser[String] = {
		(input: String, offset: Int) =>
			if (input.startsWith(s, offset)) Right(Match(s, offset + s.length))
			else buildError(input, offset, false, "'" + s + "' was expected, but '" + input.substring(offset, math.min(input.length, offset + s.length)) + "' was found")
	}

	implicit def regex(r: Regex): MyParser[String] = {
		(input: String, offset: Int) =>
			r.findFirstMatchIn(input.substring(offset)) match {
				case Some(m) => Right(Match(m.matched, offset + m.end))
				case None => buildError(input, offset, false, "A segment that matches '" + r + "' was expected, but '" + input.substring(offset, math.min(input.length, offset + r.regex.length)) + "' was found")
			}
	}

	def slice[A](pa: MyParser[A]): MyParser[String] = {
		(input: String, offset: Int) =>
			pa(input, offset).right.map { r => Match(input.substring(offset, r.nextOffset), r.nextOffset) }
	}

	def flatMap[A, B](pa: MyParser[A])(f: A => MyParser[B]): MyParser[B] = {
		(input: String, offset: Int) =>
			pa(input, offset) match {
				case Left(pe) => Left(pe)
				case Right(result) =>
					val pb = f(result.value)
					pb(input, result.nextOffset).left.map(x => Error(x.parseError, true))
			}
	}

	def label[A](text: String)(pa: MyParser[A]): MyParser[A] = {
		(input: String, offset: Int) =>
			pa(input, offset).left.map { e =>
				Error(e.parseError.updateTopMessage(text), e.commited)
			}
	}

	def scope[A](text: String)(pa: MyParser[A]): MyParser[A] = {
		(input: String, offset: Int) =>
			pa(input, offset).left.map { e =>
				Error(e.parseError.push(offset, text), e.commited)
			}
	}

	def or[A](pa: MyParser[A], pb: => MyParser[A]): MyParser[A] = {
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

	def attempt[A](pa: MyParser[A]): MyParser[A] = {
		(input: String, offset: Int) =>
			pa(input, offset).left.map { e => Error(e.parseError, false) }
	}

}
