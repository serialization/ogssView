package qq.editor.queries.parser

import scala.util.parsing.combinator._;
import scala.util.parsing.input._;
/* tokens */
sealed trait Token
case class Ident(val name: String) extends Token with Positional
case class StrLit(val text: String) extends Token with Positional
case class IntLit(val value: Integer) extends Token with Positional
case class FltLit(val value: Double) extends Token with Positional
case class ObjLit(val pool: String, val id: Integer) extends Token with Positional
case class Variable(val name: String) extends Token with Positional
/* punctuation */
case class Bra() extends Token with Positional
case class Ket() extends Token with Positional
case class Braket() extends Token with Positional
case class Squi() extends Token with Positional
case class Ggle() extends Token with Positional
case class Paren() extends Token with Positional
case class Thesis() extends Token with Positional
case class Underscore() extends Token with Positional
case class Dot() extends Token with Positional
case class Comma() extends Token with Positional
case class Semi() extends Token with Positional
case class Equals() extends Token with Positional
case class Pound() extends Token with Positional
/* keywords */
case class TypeKwd() extends Token with Positional
case class DirectTypeKwd() extends Token with Positional
case class FilterKwd() extends Token with Positional
case class UnionKwd() extends Token with Positional

object Lexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\n\f]+".r

  def bra: Parser[Bra] = "\\[".r ^^ { _ ⇒ new Bra() }
  def ket: Parser[Ket] = "\\]".r ^^ { _ ⇒ new Ket() }
  def braket: Parser[Braket] = "\\[\\]".r ^^ { _ ⇒ new Braket() }
  def squi: Parser[Squi] = "\\{".r ^^ { _ ⇒ new Squi() }
  def ggle: Parser[Ggle] = "\\}".r ^^ { _ ⇒ new Ggle() }
  def paren: Parser[Paren] = "\\(".r ^^ { _ ⇒ new Paren() }
  def thesis: Parser[Thesis] = "\\)".r ^^ { _ ⇒ new Thesis() }
  def underscore: Parser[Underscore] = "_".r ^^ { _ ⇒ new Underscore() }
  def dot: Parser[Dot] = "\\.".r ^^ { _ ⇒ new Dot() }
  def comma: Parser[Comma] = ",".r ^^ { _ ⇒ new Comma() }
  def semi: Parser[Semi] = ";".r ^^ { _ ⇒ new Semi() }
  def equals: Parser[Equals] = "=".r ^^ { _ ⇒ new Equals() }
  def pound: Parser[Pound] = "#".r ^^ { _ ⇒ new Pound() }
  def typeKwd: Parser[TypeKwd] = "type".r ^^ { _ ⇒ new TypeKwd() }
  def directTypeKwd: Parser[DirectTypeKwd] = "(?i)directtype".r ^^ { _ ⇒ new DirectTypeKwd() }
  def filterKwd: Parser[FilterKwd] = "(?i)filter".r ^^ { _ ⇒ new FilterKwd() }
  def unionKwd: Parser[UnionKwd] = "(?i)union".r ^^ { _ ⇒ new UnionKwd() }

  def ident: Parser[Ident] = ("\\w+".r ^^ { x ⇒ new Ident(x) }) | ("'[^']+'".r ^^ { x ⇒ new Ident(x.substring(1, x.length - 1)) })
  def strLit: Parser[StrLit] = "\"[^\"]*(\"\"[^\"]*)*\"".r ^^ { x ⇒ new StrLit(x.substring(1, x.length - 1).replace("\"\"", "\"")) }
  def intLit: Parser[IntLit] = "[-+]?[0-9]+".r ^^ { x ⇒ new IntLit(x.toInt) }
  def fltLit: Parser[FltLit] = "[-+]?[0-9]+\\.[0-9]+((?i)E[-+]?[0-9]+)?".r ^^ { x ⇒ new FltLit(x.toDouble) }
  def variable: Parser[Variable] = "[?$]".r ~ ident ^^ { case _ ~ x ⇒ new Variable(x.name) }
  def objLit: Parser[ObjLit] = ident ~ pound ~ intLit ^^ { case p ~ _ ~ i ⇒ new ObjLit(p.name, i.value) }

  def tokens: Parser[List[Token]] = {
    phrase(rep1(
      braket | bra | ket | squi | ggle | paren | thesis | underscore | dot | comma | semi
        | equals | typeKwd | directTypeKwd | filterKwd | unionKwd | strLit | fltLit | intLit
        | variable | objLit | ident)) ^^ (x ⇒ x)
  }

  def apply(s: String): List[Token] = {
    parse(tokens, s) match {
      case NoSuccess(msg, next)  ⇒ throw new Exception(msg)
      case Success(result, next) ⇒ result
    }
  }
}
 