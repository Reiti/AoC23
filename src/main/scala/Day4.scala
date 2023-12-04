import util.Day

import scala.annotation.tailrec

object  Day4 extends Day(4):
  override def solve(): Unit =
    val cards = inputLines.map(l => l.split(":")(1).trim.split("\\|").map(_.replaceAll(" +", " ").trim.split(" ").toSet)).map(n => (n(0), n(1)))

    //Part 1
    println(cards.map(c => c._1.intersect(c._2).size).filter(_ != 0).map(exp => Math.pow(2, exp - 1).intValue).sum)

    //Part 2
    println(calculateScratchCards(cards))

  def calculateScratchCards(cards: List[(Set[String], Set[String])]): Int =
    calculateScratchCards(cards.map(c => c._1.intersect(c._2).size), 1, (1 to cards.size).map(i => i -> 1).toMap)

  @tailrec
  def calculateScratchCards(winningCount: List[Int], pos: Int, counts: Map[Int, Int]): Int = winningCount match
    case count :: rest => calculateScratchCards(rest, pos + 1, counts.map((key, value) => if key > pos && key <= (pos + count) then (key, value + counts.getOrElse(pos, 0)) else (key, value)))
    case _ => counts.values.sum