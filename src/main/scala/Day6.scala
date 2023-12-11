import util.Day

import scala.annotation.tailrec

object Day6 extends Day(6):
  override def solve(): Unit =
    val numberRegex = raw"(\d)+".r
    
    val times = numberRegex.findAllIn(inputLines.head).map(_.toLong).toList
    val distances = numberRegex.findAllIn(inputLines(1)).map(_.toLong).toList
    
    //Part 1
    println((times zip distances).map(i => calc(i._1, i._2)).product)
    
    val time = times.map(_.toString).mkString.toLong
    val distance = distances.map(_.toString).mkString.toLong
    
    //Part 2
    println(calc(time, distance))

  @tailrec
  def calc(time: Long, distance: Long, count: Long = 0, spent: Long = 0): Long =
    if spent == time then
      count
    else if ((time - spent) * spent) > distance then
      calc(time, distance, count + 1, spent + 1)
    else
      calc(time, distance, count, spent + 1)

