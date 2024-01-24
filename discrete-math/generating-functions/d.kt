import kotlin.math.max
import kotlin.math.min
import kotlin.math.pow

private fun readInt() = readln().toInt()
private fun readInts() = readln().split(' ').map { it.toInt() }
private fun readLongs() = readln().split(' ').map { it.toLong() }
private fun factorial(num: Long): Long =
    when {
        (num == 0L) -> 1
        (num > 0L) -> num * factorial(num - 1)
        else -> -1
    }
private fun gcd(a: Long, b: Long): Long =
    when {
        (b == 0L) -> a
        else -> gcd(b, a % b)
    }

private fun polyMultiply(a: List<Long>, b:List<Long>): ArrayList<Long> {
    val result = ArrayList<Long>()
    val n = a.size - 1
    val m = b.size - 1
    for (i in 0..n + m) {
        var tmp = 0L
        for (j in 0..i) {
            val s: Long = if (j <= n && i - j <= m) a[j] * b[i - j] else 0L
            tmp += s
        }
        result.add(tmp)
    }
    return result
}

private fun polyAdd(a: List<Long>, b: List<Long>):ArrayList<Long> {
    val result = ArrayList<Long>()
    val n = a.size - 1
    val m = b.size - 1
    for (i in 0..max(n, m)) {
        val tmp = if (i <= min(n, m)) a[i] + b[i] else if (i <= n) a[i] else b[i]
        result.add(tmp)
    }
    return result
}

fun main() {
    val (r, k)  = readInts()
    val pks = readInts()

    val resultTmp = ArrayList<ArrayList<Long>>()

    for (i in 0 until pks.size) {
        val tmp = ArrayList<ArrayList<Long>>()
        for (j in 0 until k) {
            val tmp2 = ArrayList<Long>()
            val last = (-i + k - j).toLong()
            tmp2.add(last)
            tmp2.add(1.toLong())
            tmp.add(tmp2)
        }
        val times = tmp.reduce { a, b ->  polyMultiply(a, b) }
        val times2 = polyMultiply(times, listOf(pks[i].toLong() * r.toDouble().pow((k - i).toDouble()).toLong()))
        resultTmp.add(times2)
    }
    val res = resultTmp.reduce { a, b ->  polyAdd(a, b) }
    val denominator = factorial(k.toLong()) * r.toDouble().pow(k.toDouble()).toLong()

    res.forEach {
        var div = gcd(it, denominator)
        div = if (div > 0) div else -div
        print("${it/div}/${denominator/div} ")
    }
}
