object Demo {
  def main(args: Array[String]) {
    var a = 60;           /* 60 = 0011 1100 */
    var b = 13;           /* 13 = 0000 1101 */
    var c = 0;

    /**
     * a & b take positive in both
     * a | b take positive in a or b
     * a ^ b take exclusive or
     * ~a invert a
     * a << 2 shift bits left with 2 places new ones are 0 (equal to a**2)
     * a >> 2 move bits 2 positions to right dropping leading zero's
     * a >> 2 move bits 2 positions to right keeping leading zeros
     */
    c = a & b;            /* 12 = 0000 1100 */
    println("a & b = " + c );

    c = a | b;            /* 61 = 0011 1101 */
    println("a | b = " + c );

    c = a ^ b;            /* 49 = 0011 0001 */
    println("a ^ b = " + c );

    c = ~a;               /* -61 = 1100 0011 */
    println("~a = " + c );

    c = a << 2;           /* 240 = 1111 0000 */
    println("a << 2 = " + c );

    c = a >> 2;           /* 215 = 1111 */
    println("a >> 2  = " + c );

    c = a >>> 2;          /* 215 = 0000 1111 */
    println("a >>> 2 = " + c );
  }
}