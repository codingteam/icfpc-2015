package ru.org.codingteam.icfpc

class PRNG(seed: Long) extends BufferedIterator[Int] {
    var state = seed

    val MODULO: Long = math.pow(2, 32).toLong
    val MULTIPLIER: Long = 1103515245
    val INCREMENT: Long = 12345

    def hasNext: Boolean = true

    def next_state: Long = (MULTIPLIER * state + INCREMENT) % MODULO

    def to_result(state: Long): Int =
        ((state >> 16) & (math.pow(2, 15).toLong - 1)).toInt

    def head: Int = to_result(state)

    def next: Int = {
        val result = to_result(state)
        state = next_state
        result
    }
}
