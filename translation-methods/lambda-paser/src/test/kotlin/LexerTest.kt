import org.junit.jupiter.api.assertThrows
import kotlin.test.*

class LexerTest {

    @Test
    fun testEmptyString() {
        val res = getTokens("")

        assertEquals(res.size, 1)
        assertEquals(res[0], TokenId.END)
    }

    @Test
    fun testBasicLambdas() {
        val test1 = "lambda x : x + 2"
        val test2 = "lambda x, y, z : x + y * z"

        val expected1 = listOf(TokenId.LAMBDA, TokenId.ID,
            TokenId.COLON, TokenId.ID, TokenId.PLUS, TokenId.CONST, TokenId.END)
        val expected2 = listOf(TokenId.LAMBDA, TokenId.ID, TokenId.COMMA, TokenId.ID,
            TokenId.COMMA, TokenId.ID, TokenId.COLON, TokenId.ID,
            TokenId.PLUS, TokenId.ID, TokenId.TIMES, TokenId.ID, TokenId.END)
        assertEquals(expected1, getTokens(test1))
        assertEquals(expected2, getTokens(test2))
    }

    @Test
    fun testBadLambdas() {
        val test1 = "lam x : x - 2"
        val expected1 = listOf(TokenId.ID, TokenId.ID, TokenId.COLON, TokenId.ID,
            TokenId.MINUS, TokenId.CONST, TokenId.END)

        val test2 = "lambda x : x _ 2"
        val test3 = "lambda x, y : x \\ y"

        assertEquals(expected1, getTokens(test1))
        assertThrows<Exception> { getTokens(test2) }
        assertThrows<Exception> { getTokens(test3) }
    }

    @Test
    fun testWhitespaceSkipper() {
        val test1 = "     lambda    x,    y    : x    -    y    "
        val expected1 = listOf(TokenId.LAMBDA, TokenId.ID, TokenId.COMMA, TokenId.ID,
            TokenId.COLON, TokenId.ID, TokenId.MINUS, TokenId.ID, TokenId.END)

        val test2 = "    lambda x,y: x-y"
        val expected2 = listOf(TokenId.LAMBDA, TokenId.ID, TokenId.COMMA, TokenId.ID, TokenId.COLON,
            TokenId.ID, TokenId.MINUS, TokenId.ID, TokenId.END)

        assertEquals(expected1, getTokens(test1))
        assertEquals(expected2, getTokens(test2))
    }

    private fun getTokens(string: String): List<TokenId> {
        val lexer = Lexer(string)
        var token = lexer.scan()
        val listOfTokens = arrayListOf(token.tag)

        if (token.tag == TokenId.END) {
            return listOfTokens
        }
        while (true) {
            token = lexer.scan()
            listOfTokens.add(token.tag)
            if (token.tag == TokenId.END) {
                break
            }
        }
        return listOfTokens
    }
}
