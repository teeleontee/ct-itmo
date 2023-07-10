
private fun readInt() = readln().toInt()
private fun readInts() = readln().split(' ').map {it.toInt()}
private fun readLongs() = readln().split(' ').map {it.toLong()}
private fun timesMod(a: Long): Long = Math.floorMod(a, 104857601).toLong()
private fun addMod(a: Long, b: Long): Long = (a + b) %  104857601

private fun matrixMultiply(a: List<Long>, b:List<Long>): ArrayList<Long> {
    val result = ArrayList<Long>()
    val n = a.size - 1
    val m = b.size - 1
    for (i in 0..n + m) {
        var tmp = 0L
        for (j in 0..i) {
            val s = if (j <= n && i - j <= m) timesMod(a[j] * b[i - j]) else 0L
            tmp = addMod(tmp, s)
        }
        result.add(tmp)
    }
    return result
}

private fun getInverted(a: List<Long>): ArrayList<Long> {
    val result = ArrayList<Long>()
    result.add(1)
    for (i in 1 until a.size) {
        if (i % 2 == 1) result.add(-a[i])
        else result.add(a[i])
    }
    return result
}

private fun getRoot(a: List<Long>): ArrayList<Long> {
    val result = ArrayList<Long>()
    result.addAll(a.filterIndexed { index, _ ->  index % 2 == 0})
    return result
}

fun main() {
    var (k1, n) = readLongs()
    val k = k1.toInt()
    n -= 1
    var ass = ArrayList<Long>()
    ass.addAll(readLongs())
    val cs = ArrayList<Long>()
    cs.addAll(readLongs())


    var qs = ArrayList<Long>()
    qs.add(1)
    for (i in 0 until cs.size) {
        qs.add(-cs[i])
    }

    while (n >= k) {
        for (i in k..2 * k) {
            var tmp = 0L
            for (j in 1..k) {
                val t: Long = timesMod(-1 * qs[j])
                tmp = addMod(tmp, timesMod(t * ass[ass.size - j]))
            }
            ass.add(tmp)
        }
        val r = matrixMultiply(qs, getInverted(qs))
        ass = ass.filterIndexed { index, _ ->  index.toLong() % 2 == n % 2 } as ArrayList<Long>
        qs = getRoot(r)
        n /= 2
    }
    print(ass[n.toInt()])
}
