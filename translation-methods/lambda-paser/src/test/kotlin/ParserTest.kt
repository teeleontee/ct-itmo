import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotEquals

class ParserTest {
    private val parser = Parser()

    /**
     * Smoke test
     */
    @Test
    fun testBasicLambdas() {
        val test1 = "lambda x : x + 2"
        val test2 = "lambda x, y : x * y"

        assertNotEquals(null, parser.parse(test1))
        assertNotEquals(null, parser.parse(test2))
    }

    /**
     * We have the production rule `P -> id P'`, `P' -> , id P'` and `P' -> eps`, this makes sure that
     * valid sequences pass
     */
    @Test
    fun testParametersPassing() {
        val passingTests = arrayListOf(
        "lambda x : x + 2",
        "lambda x, y: x + 2",
        "lambda x, y, z : x + 2",
        "lambda x, y, z, p: x + 2",
        "lambda x, y, z, p, q: x + 2",
        "lambda x, y, z, p, q, t: x + 2",
        "lambda x, y, z, p, q, t, l: x + 2")

        passingTests.forEach {
            assertNotEquals(null, parser.parse(it))
        }
    }

    /**
     * We have the production rule `P -> id P'`, `P' -> , id P'` and `P' -> eps`, this makes sure that
     * invalid sequences fail
     */
    @Test
    fun testParametersFailing() {
        val failingTests = arrayListOf(
            "lambda x, : x + 2",
            "lambda x, y,  : x + 2",
            "lambda x,y,z, : x + 2",
        )

        failingTests.forEach {
            assertEquals(null, parser.parse(it))
        }
    }

    /**
     * Testing basic expression operations on valid lambdas. These test also cover `F -> ( E )`
     */
    @Test
    fun testExpressionsPassing() {
        val passingTests = arrayListOf(
            "lambda x, y : x + y * 2",
            "lambda x, y : x * y - 2",
            "lambda x : x + 2 + 2 + 2",
            "lambda x : x - y * 2 + 5",
            "lambda x : x - (y * 2 + 5)",
            "lambda x : x + (5)",
            "lambda x, y, z: (x + y) * z - (y - x) * z"
        )

        passingTests.forEach {
           assertNotEquals(null, parser.parse(it))
        }
    }

    /**
     * Testing bad lambdas in expression. These cover from incorrect braces to invalid productions
     */
    @Test
    fun testExpressionsFailing() {
        val failingTests = arrayListOf(
            "lambda x, y : x - y (x - 5)",
            "lambda x, y : + x y - 5",
            "lambda x : x + (5",
            "lambda x, y : x - (5 - 5))",
            "lambda x, y : x - 5))",
            "lambda x, y : x ++ y",
        )

        failingTests.forEach {
            assertEquals(null, parser.parse(it))
        }
    }
}