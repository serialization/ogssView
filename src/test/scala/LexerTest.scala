
import org.scalatest;
import qq.editor.queries.parser._;

class LexerTest extends scalatest.FunSuite {
  test("tokens are parsed as expected") {
    assert(Lexer("(").head.isInstanceOf[Paren])
    assert(Lexer(")").head.isInstanceOf[Thesis])
    assert(Lexer("[").head.isInstanceOf[Bra])
    assert(Lexer("]").head.isInstanceOf[Ket])
    assert(Lexer("[]").head.isInstanceOf[Braket])
    assert(Lexer("{").head.isInstanceOf[Squi])
    assert(Lexer("}").head.isInstanceOf[Ggle])
    assert(Lexer(".").head.isInstanceOf[Dot])
    assert(Lexer(",").head.isInstanceOf[Comma])
    assert(Lexer(";").head.isInstanceOf[Semi])
    assert(Lexer("_").head.isInstanceOf[Underscore])
    assert(Lexer("'_'").head.asInstanceOf[Ident].name == "_")
    assertThrows[Exception](Lexer("#")) // Pound token is only used internally to build object literals from identifiers and keywords
    assert(Lexer("=").head.isInstanceOf[Equals])
    assert(Lexer("/=").head.isInstanceOf[NEquals])
    assert(Lexer("<=").head.isInstanceOf[LTE])
    assert(Lexer("<").head.isInstanceOf[LT])
    assert(Lexer(">=").head.isInstanceOf[GTE])
    assert(Lexer(">").head.isInstanceOf[GT])
    assert(Lexer("type").head.isInstanceOf[TypeKwd])
    assert(Lexer("'type'").head.asInstanceOf[Ident].name == "type")
    assert(Lexer("directType").head.isInstanceOf[DirectTypeKwd])
    assert(Lexer("union").head.isInstanceOf[UnionKwd])
    assert(Lexer("filter").head.isInstanceOf[FilterKwd])
    assert(Lexer("false").head.isInstanceOf[FalseKwd])
    assert(Lexer("true").head.isInstanceOf[TrueKwd])
    assert(Lexer("identifier").head.asInstanceOf[Ident].name == "identifier")
    assert(Lexer("ident1fier").head.asInstanceOf[Ident].name == "ident1fier")
    assert(Lexer("\\u0031dentifier").head.asInstanceOf[Ident].name == "1dentifier")
    assert(Lexer("'1dentifier'").head.asInstanceOf[Ident].name == "1dentifier")
    assert(Lexer("ιδεντ").head.asInstanceOf[Ident].name == "ιδεντ")
    assert(Lexer("cộm͜bi̋nn̸e̛s").head.asInstanceOf[Ident].name == "cộm͜bi̋nn̸e̛s")
    assert(Lexer("_ a").head.isInstanceOf[Underscore])
    assert(Lexer("_a").head.asInstanceOf[Ident].name == "_a")
    assert(Lexer("?var").head.asInstanceOf[Variable].name == "var")
    assert(Lexer("$var").head.asInstanceOf[Variable].name == "var")
    assert(Lexer("pool#1").head.asInstanceOf[ObjLit].pool == "pool")
    assert(Lexer("pool#1").head.asInstanceOf[ObjLit].id == 1)
    assert(Lexer("'type'#1").head.asInstanceOf[ObjLit].pool == "type")
    assert(Lexer("'1'#1").head.asInstanceOf[ObjLit].pool == "1")
    assertThrows[Exception](Lexer("pool#"))
    assert(Lexer("1").head.asInstanceOf[IntLit].value == 1)
    assert(Lexer("+42").head.asInstanceOf[IntLit].value == 42)
    assert(Lexer("-1").head.asInstanceOf[IntLit].value == -1)
    assert(Lexer("1.").length == 2)
    assert(Lexer(".5").length == 2)
    assert(Lexer("-1.5E").length == 2)
    assertThrows[Exception](Lexer("-1.5E+"))
    assertThrows[Exception](Lexer("-+1"))
    assert(Lexer("3.14").head.asInstanceOf[FltLit].value == 3.14)
    assert(Lexer("-0.5").head.asInstanceOf[FltLit].value == -0.5)
    assert(Lexer("0 . 5").length == 3)
    assert(Lexer("+1.6021766208E-19").head.asInstanceOf[FltLit].value == 1.6021766208e-19) // ±0.0000000098e-19
    assert(Lexer("\"string\"").head.asInstanceOf[StrLit].text == "string")
    assert(Lexer("\"\"\"is false when preceeded by its own quotation\"\" is false when preceeded by its own quotation\"").
      head.asInstanceOf[StrLit].text == "\"is false when preceeded by its own quotation\" is false when preceeded by its own quotation")

  }
}