import kotlin.collections.ArrayList

private fun readLongs() = readln().split(' ').map { it.toLong() }
private fun readInt() = readln().toInt()

fun main() {
    val k = readInt()
    val arr = ArrayList<Long>()
    val cs = ArrayList<Long>()
    cs.add(-1)
    arr.addAll(readLongs())
    cs.addAll(readLongs())

    val ps = ArrayList<Long>()
    ps.add(arr[0])
    for (i in 1 until k) {
        var tmp: Long = 0
        for (j in 1 until k) {
            val s = if (i - j >= 0) arr[i - j] * cs[j] else 0
            tmp += s
        }
        val p = arr[i] - tmp
        ps.add(p)
    }

    val qs = ArrayList<Long>()
    cs.forEach { qs.add(it * -1) }

    for (i in ps.size-1 downTo 0) {
        if (ps[i].toInt() == 0) {
            ps.removeAt(i)
        } else {
            break
        }
    }
    println(ps.size - 1)
    ps.forEach { print("$it ") }
    println()
    println(qs.size - 1)
    qs.forEach { print("$it ") }
}
