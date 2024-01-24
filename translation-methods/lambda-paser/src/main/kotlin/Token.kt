
sealed class Token(val tag: TokenId) {
    fun print(): String {
        println(tag)
        return tag.toString()
    }
}

class Keyword(lexeme: TokenId): Token(lexeme)

class OperationSymbol(lexeme: TokenId): Token(lexeme)

class KeySymbol(lexeme: TokenId): Token(lexeme)

class Variable(private val name: String) : Token(TokenId.ID)

class Constant(private val value: Int) : Token(TokenId.CONST)

class EndSymbol : Token(TokenId.END)