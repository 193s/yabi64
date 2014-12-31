package yaib64

import scala.io.StdIn.readLine

object Base64 {
  def main(args: Array[String]) {
    val s = readAll()
    println(base64encode(s))
  }

  def base64encode(s: String) =
    (s.map(binaryString).mkString+"0000").grouped(6).
      map(binary2int).map(base64_table).mkString.
      grouped(4).map(s => s + base64_table(-1).toString*(4-s.length)).mkString


  val base64_table = Map (
    -1 -> '=',
    0 -> 'A',
    1 -> 'B',
    2 -> 'C',
    3 -> 'D',
    4 -> 'E',
    5 -> 'F',
    6 -> 'G',
    7 -> 'H',
    8 -> 'I',
    9 -> 'J',
    10 -> 'K',
    11 -> 'L',
    12 -> 'M',
    13 -> 'N',
    14 -> 'O',
    15 -> 'P',
    16 -> 'Q',
    17 -> 'R',
    18 -> 'S',
    19 -> 'T',
    20 -> 'U',
    21 -> 'V',
    22 -> 'W',
    23 -> 'X',
    24 -> 'Y',
    25 -> 'Z',
    26 -> 'a',
    27 -> 'b',
    28 -> 'c',
    29 -> 'd',
    30 -> 'e',
    31 -> 'f',
    32 -> 'g',
    33 -> 'h',
    34 -> 'i',
    35 -> 'j',
    36 -> 'k',
    37 -> 'l',
    38 -> 'm',
    39 -> 'n',
    40 -> 'o',
    41 -> 'p',
    42 -> 'q',
    43 -> 'r',
    44 -> 's',
    45 -> 't',
    46 -> 'u',
    47 -> 'v',
    48 -> 'w',
    49 -> 'x',
    50 -> 'y',
    51 -> 'z',
    52 -> '0',
    53 -> '1',
    54 -> '2',
    55 -> '3',
    56 -> '4',
    57 -> '5',
    58 -> '6',
    59 -> '7',
    60 -> '8',
    61 -> '9',
    62 -> '+',
    63 -> '/'
  )

  private def binary2int(s: String) = Integer.parseInt(s, 2)

  private def readAll() =
    Iterator.continually(readLine()).takeWhile(_ != null).mkString("\n")
  private def binaryString(b: Char) =
    String.format("%8s", (b.toByte & 0xFF).toBinaryString).replace(' ', '0')
}
