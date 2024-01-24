import java.io.IOException
import java.util.HashMap

/**
* src.Lexer class - Takes in a string and splits it into tokens
* according to the grammar of lambda functions
* @param str The string that is to be split into tokens
*/
class Lexer(private var str: String) {
    private val keyMap: HashMap<String, TokenId> = HashMap()
    private var curSymbol: Char = ' '
    private var index = 0

    init {
        keyMap["lambda"] = TokenId.LAMBDA
        str += "$"
    }

    private fun getNextSymbol(i: Int): Char {
        index++
        return if (i >= str.length) {
            '$'
        } else {
            str[i]
        }
    }

    private fun skipWhitespace() {
        while (true) {
            curSymbol = getNextSymbol(index)
            if (!curSymbol.isWhitespace()) {
                break
            }
        }
    }

    private fun readNumber(): Token {
        var num = 0
        while (Character.isDigit(curSymbol)) {
            num = num * 10 + curSymbol.digitToInt()
            curSymbol = getNextSymbol(index)
        }
        index--
        return Constant(num)
    }

    private fun readWord(): Token {
        var word = ""
        while (Character.isLetter(curSymbol)) {
            word += curSymbol
            curSymbol = getNextSymbol(index)
        }
        index--
        return if (keyMap.containsKey(word)) {
            Keyword(keyMap[word]!!)
        } else {
            Variable(word)
        }
    }

    private fun readOperation(): Token {
        val opToken = when (curSymbol) {
            '*' -> TokenId.TIMES
            '/' -> TokenId.DIVIDE
            '+' -> TokenId.PLUS
            '-' -> TokenId.MINUS
            else -> {
                throw Exception("invalid op")
            }
        }
        return OperationSymbol(opToken)
    }


    private fun readKeySymbols(): Token {
        val token = when(curSymbol) {
            ':' -> TokenId.COLON
            ',' -> TokenId.COMMA
            else -> {
                throw Exception("unknown keySymbol")
            }
        }
        return KeySymbol(token)
    }

    private fun readColons() : Token {
        val token = when (curSymbol) {
            '(' -> TokenId.LPAREN
            ')' -> TokenId.RPAREN
            else -> throw Exception("asd")
        }
        return KeySymbol(token)
    }

    private fun Char.isOperation(): Boolean {
        return when (this) {
            '+','-','*','/' -> true
            else -> false
        }
    }

    private fun Char.isColon(): Boolean {
        return when (this) {
            '(', ')' -> true
            else -> false
        }
    }

    private fun Char.isKeySymbol(): Boolean {
        return when (this) {
            ':',',', -> true
            else -> false
        }
    }

    /**
     * Scans for the first token available
     * after curSymbol, skips whitespaces
     * @throws IOException if token cannot be read
     * @return Token
     */
    @Throws(IOException::class)
    fun scan(): Token {
        skipWhitespace()
        return when {
            curSymbol.isDigit()     -> readNumber()
            curSymbol.isLetter()    -> readWord()
            curSymbol.isColon()     -> readColons()
            curSymbol.isOperation() -> readOperation()
            curSymbol.isKeySymbol() -> readKeySymbols()
            curSymbol == '$'        -> EndSymbol()
            else -> {
                throw Exception("something went wrong while parsing, bad symbol $curSymbol at index : $index")
            }
        }
    }
}
