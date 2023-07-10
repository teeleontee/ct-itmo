import kotlin.math.pow

private fun readLongs() = readln().split(' ').map { it.toLong() }
private fun readInts() = readln().split(' ').map { it.toInt() }

val mod: Long = 10.toDouble().pow(9).toLong() + 7

private fun timesMod(a: Long): Long = Math.floorMod(a, mod)
private fun addMod(a: Long, b: Long): Long = (a + b) % mod

private fun getAnsI(n: Int, tmp: List<Long>, weights: List<Long>): Long {
    var ans = 0L
    for (i in weights) {
        if (n - i < tmp.size && n - i >= 0)
            ans = addMod(tmp[n - i.toInt()], ans)
    }
    return ans
}

private fun getTmpI(n: Int, ans: List<Long>): Long {
    var tmp = 0L
    for (i in 0..n) {
        if (i < ans.size && n - i < ans.size)
            tmp = addMod(tmp, timesMod(ans[i] * ans[n - i]))
    }
    return tmp
}

fun main() {
    val (k, m) = readInts()
    val cs = readLongs()

    val ans = ArrayList<Long>()
    val temp = ArrayList<Long>()

    ans.add(1)
    temp.add(1)

    for (i in 1..m) {
        ans.add(getAnsI(i, temp, cs))
        temp.add(getTmpI(i, ans))
    }

    ans.subList(1, ans.size).forEach { print("${Math.floorMod(it, mod)} ") }
}
