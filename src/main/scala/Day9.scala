import util.Day

import scala.annotation.tailrec

object Day9 extends Day(9):
  override def solve(): Unit =
    val reports = inputLines.map(_.split(" ").map(_.toInt).toList)

    //Part 1
    println(reports.map(r => extrapolate(differences(r))).sum)

    //Part 2
    println(reports.map(r => extrapolate(differences(r.reverse))).sum)

  @tailrec
  def differences(report: List[Int], acc: List[List[Int]] = List()): List[List[Int]] =
    if report.forall(_ == 0) then
      report.appended(0) :: acc
    else
      val diff = report.sliding(2).map(g => g(1) - g.head).toList
      differences(diff, report :: acc)

  @tailrec
  def extrapolate(differences: List[List[Int]]): Int = differences match
    case x :: Nil => x.last
    case x :: xs => extrapolate(xs.head.appended(x.last + xs.head.last) :: xs.tail)
    case _ => assert(false)