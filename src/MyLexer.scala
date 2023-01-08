
class MyLexer {

  // define the tokens recognized by the lexer
  sealed trait Token
  case class IntegerToken(value: Int) extends Token
  case class PlusToken() extends Token
  case class MultToken() extends Token
  case class OpenParensToken() extends Token
  case class CloseParensToken() extends Token

  def lex(code: String): Either[String, List[Token]] = {
    // a mutable list to store the tokens
    val tokens = scala.collection.mutable.ListBuffer[Token]()
    // a mutable variable to store the current token being built
    var currentToken = new StringBuilder

    // iterate over the characters in the code
    for (c <- code) {
      c match {
        // if the character is a digit, add it to the current token
        case c if c.isDigit => currentToken.append(c)
        // if the character is a plus sign, create a PlusToken and reset the current token
        case '+' =>
          if (currentToken.nonEmpty) {
            tokens += IntegerToken(currentToken.toString.toInt)
            currentToken.clear()
          }
          tokens += PlusToken()
        case '*' =>
          if (currentToken.nonEmpty) {
            tokens += IntegerToken(currentToken.toString().toInt)
            currentToken.clear()
          }
          tokens += MultToken()
        case '(' =>
          if (currentToken.nonEmpty) {
            tokens += IntegerToken(currentToken.toString().toInt)
            currentToken.clear()
          }
          tokens += OpenParensToken()
        case ')' =>
          if (currentToken.nonEmpty) {
            tokens += IntegerToken(currentToken.toString().toInt)
            currentToken.clear()
          }
          tokens += CloseParensToken()
        // if the character is something else, return an error
        case _ => return Left(s"Invalid character: $c")
      }
    }

    // if there is a current token when the loop ends, it must be an integer
    if (currentToken.nonEmpty) {
      tokens += IntegerToken(currentToken.toString.toInt)
    }

    // return the list of tokens
    Right(tokens.toList)
  }

  sealed trait Tree
  case class Node(value: Any, children: List[Tree]) extends Tree
  case class Leaf(value: Any) extends Tree

  def parseExpr(tokens: List[Token]): (Tree, List[Token]) = {
    val (t, rest) = parseTerm(tokens)
    rest match {
      case PlusToken() :: rest =>
        val (e, rest1) = parseExpr(rest)
        (Node(PlusToken(), List(t, e)), rest1)
      case _ => (t, rest)
    }
  }

  def parseTerm(tokens: List[Token]): (Tree, List[Token]) = {
    val (f, rest) = parseFactor(tokens)
    rest match {
      case MultToken() :: rest1 =>
        val (t, rest: List[Token]) = parseTerm(rest1)
        (Node(MultToken(), List(f, t)), rest)
      case _ => (f, rest)
    }
  }

  def parseFactor(tokens: List[Token]): (Tree, List[Token]) = tokens match {
    case OpenParensToken() :: rest =>
      val (expr, rest1) = parseExpr(rest)
      rest1 match {
        case CloseParensToken() :: rest => (expr, rest)
        case _ => throw new Exception
      }
    case IntegerToken(value) :: rest => (Leaf(value), rest)
    case _ => throw new Exception
  }


  def generateCode(tree: Tree): String = tree match {
    case Leaf(value) => value.toString
    case Node(PlusToken(), List(left, right)) =>
      val leftCode = left match {
        case Leaf(_) => generateCode(left)
        case _ => s"(${generateCode(left)})"
      }
      val rightCode = right match {
        case Leaf(_) => generateCode(right)
        case _ => s"(${generateCode(right)})"
      }
      s"$leftCode + $rightCode"
    case Node(MultToken(), List(left, right)) =>
      val leftCode = left match {
        case Leaf(_) => generateCode(left)
        case _ => s"(${generateCode(left)})"
      }
      val rightCode = right match {
        case Leaf(_) => generateCode(right)
        case _ => s"(${generateCode(right)})"
      }
      s"$leftCode * $rightCode"
    case _ => throw new Exception
  }

}


object Lexer1 {

  def main(args: Array[String]): Unit = {
    val lex = new MyLexer

    val tokens = lex.lex("(1+2)*2+8")
    val (output, rest) = lex.parseExpr(tokens.getOrElse(throw new Exception()))
    val res = lex.generateCode(output)
    print(res)
  }

}