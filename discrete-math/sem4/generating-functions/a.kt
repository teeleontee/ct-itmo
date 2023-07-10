import kotlin.math.max
import kotlin.math.min

private fun readLong() = readln().split(' ').map { it.toLong() }
private fun readInt() = readln().split(' ').map { it.toInt() }

private fun timesMod(a: Long) = Math.floorMod(a, 998244353)
private fun addMod(a: Long, b: Long) = (a + b) % 998244353

fun main() {
    val (n, m) = readInt()
    val ps = ArrayList<Long>()
    val qs = ArrayList<Long>()
    val adds = ArrayList<Long>()
    ps.addAll(readLong())
    qs.addAll(readLong())

    for (i in 0..max(n, m)) {
        val tmp = if (i <= min(n, m)) addMod(ps[i] , qs[i]) else if (i <= n) ps[i] else qs[i]
        adds.add(tmp)
    }

    println(adds.size - 1)
    adds.forEach { print("$it ")  }
    println()

    val mults = ArrayList<Long>()
    for (i in 0..n + m) {
        var tmp: Long = 0
        for (j in 0..i) {
            val s = if (j <= n && i - j <= m) timesMod(ps[j] * qs[i -j]) else 0
            tmp = addMod(tmp, s.toLong())
        }
        mults.add(tmp)
    }

    println(mults.size - 1)
    mults.forEach { print("$it ") }
    println()

    val cs = ArrayList<Long>()
    val divs = ArrayList<Long>()
    qs.forEach { cs.add(it) }

    for (i in 0..999) {
        if (i == 0) {
            divs.add(ps[0])
            continue
        }
        var tmp: Long = 0
        for (j in 1 until cs.size) {
            val s = if (i - j >= 0) timesMod(divs[i - j] * cs[j] * -1)  else 0
            tmp = addMod(tmp, s.toLong())
        }
        if (i < ps.size) { tmp = addMod(tmp, ps[i]) }
        divs.add(tmp)
    }

    divs.forEach { print("$it ") }
    println()
}