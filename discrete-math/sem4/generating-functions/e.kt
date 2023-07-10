import kotlin.math.pow

private fun readInt() = readln().toInt()
private fun readInts() = readln().split(' ').map {it.toInt()}

private fun matrixMultiply(a: List<Long>, b:List<Long>): ArrayList<Long> {
    val result = ArrayList<Long>()
    val n = a.size - 1
    val m = b.size - 1
    for (i in 0..n + m) {
        var tmp = 0L
        for (j in 0..i) {
            val s = if (j <= n && i - j <= m) a[j] * b[i - j] else 0
            tmp += s
        }
        result.add(tmp)
    }
    return result
}

fun main() {
    val r  = readInt()
    val degp = readInt()
    val ps = readInts()

    val ass = ArrayList<Long>()

    for (i in 0 .. degp)  {
        var cur = 0L
        for (j in 0 .. degp) {
            cur +=  (i.toDouble().pow(j) * ps[j]).toLong()
        }
        ass.add(cur * r.toDouble().pow(i).toLong())
    }

    var qs = ArrayList<Long>()
    qs.add(1.toLong())
    qs.add(-r.toLong())
    val qsOrig = ArrayList<Long>()
    qsOrig.add(1.toLong())
    qsOrig.add(-r.toLong())
    for (i in 1 until degp + 1) {
        qs = matrixMultiply(qs, qsOrig)
    }

    val numerator = matrixMultiply(ass, qs)

    println(degp)
    for (i in 0 .. degp) {
        print("${numerator[i]} ")
    }
    println()
    println(qs.size - 1)
    qs.forEach { print("$it ") }
}