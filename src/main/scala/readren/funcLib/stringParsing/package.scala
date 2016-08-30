package readren.funcLib

package object stringParsing  {

	case class Layer(offset: Int, message: String, branch: List[Layer])

	case class ParseError(stack: List[Layer]) {
		def top = stack.head
		def push(offset: Int, message: String): ParseError =
			ParseError(Layer(offset, message, Nil) :: stack)
		def updateTopMessage(newMessage: String): ParseError =
			ParseError(stack.head.copy(message = newMessage) :: stack.tail)
		def updateTopBranch(newBrach: ParseError): ParseError = {
			ParseError(stack.head.copy(branch = newBrach.stack) :: stack.tail)
		}
	}

}