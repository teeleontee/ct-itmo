import kotlin.test.Test
import kotlin.test.assertEquals

class CalcTest {
    private val parser = CalcParser()

    @Test
    fun testSmoke() {
        val test1 = parser.parse("2 + 2") // 4
        val test2 = parser.parse("2 + 3 * 4") // 14

        assertEquals(test1.attr, 4)
        assertEquals(test2.attr, 14)
    }

    @Test
    fun testAddAndSub() {
        val test1 = parser.parse("1 + 2 + 3 + 4 + 5") // 15
        val test2 = parser.parse("1 - 2 - 3 - 4 - 5") // -13
        val test3 = parser.parse("1 + 2 - 3 + 4 - 5") // -1

        assertEquals(test1.attr, 15)
        assertEquals(test2.attr, -13)
        assertEquals(test3.attr, -1)
    }

    @Test
    fun testMulAndDiv() {
        val test1 = parser.parse("1 * 2 * 3 * 4 * 5") // 120
        val test2 = parser.parse("10 / 2") // 5
        val test3 = parser.parse("6 / 2 * 3") // 9

        assertEquals(test1.attr, 120)
        assertEquals(test2.attr, 5)
        assertEquals(test3.attr, 9)
    }

    @Test
    fun testCompound() {
        val test1 = parser.parse("2 * (3 + 7)") // 20
        val test2 = parser.parse("10 / (1 + 4)") // 2
        val test3 = parser.parse("2 - (1 - 3)") // 4

        assertEquals(test1.attr, 20)
        assertEquals(test2.attr, 2)
        assertEquals(test3.attr, 4)
    }

}
