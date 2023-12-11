import util.{Day, Util}

import scala.annotation.tailrec

object Day11 extends Day(11):
  override def solve(): Unit =
    val galaxies = inputMap.filter((_, c) => c == '#').keys.toList

    //Part 1
    println(galaxies.combinations(2).map(d => Util.manhattan(d.head, d.last) + countEmpty(inputLines, d.head, d.last)).sum)

    //Part 2
    println(galaxies.combinations(2).map(d => Util.manhattan(d.head, d.last).toLong + countEmpty(inputLines, d.head, d.last) * 999999L).sum)

  def countEmpty(map: List[String], from: (Int, Int), to: (Int, Int)): Int =
    val emptyH = map.slice(Math.min(from._1, to._1), Math.max(from._1, to._1)).count(s => s.forall(_ == '.'))
    val emptyV = map.transpose.map(_.mkString).slice(Math.min(from._2, to._2), Math.max(from._2, to._2)).count(s => s.forall(_ == '.'))
    emptyH + emptyV