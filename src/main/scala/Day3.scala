import util.Day
import util.Util.mooreNeighborhood

import scala.annotation.tailrec

object Day3 extends Day(3):
  override def solve(): Unit =
    val partNumbers = findNumbers(0, 0, inputLines.toArray)

    //Part 1
    println(partNumbers.filter(_._2.nonEmpty).map(_._1).sum)

    val inverse = partNumbers.flatMap((num, symbols) => symbols.map(s => s -> num))
    val gears = inverse.map(_._1).distinct.filter(symbol => symbol._1 == '*' && inverse.count(_._1 == symbol) == 2)

    //Part 2
    println(gears.map(g => inverse.filter(_._1 == g).map(_._2).product).sum)

  @tailrec
  def findNumbers(row: Int, col: Int, grid: Array[String], found: List[(Int, Set[(Char, (Int, Int))])] = List()): List[(Int, Set[(Char, (Int, Int))])] =
    if row == grid.length then
      found
    else if !grid(row)(col).isDigit then
      if (col + 1) == grid(0).length then
        findNumbers(row + 1, 0, grid, found)
      else
        findNumbers(row, col + 1, grid, found)
    else
      val (num, symbols) = parseNumber(row, col, grid)
      val (nr, nc) = if col + num.length >= grid(0).length then (row + 1, 0) else (row, col + num.length)
      findNumbers(nr, nc, grid, (num.toInt, symbols) :: found)

  @tailrec
  def parseNumber(row: Int, col: Int, grid: Array[String], parsed: String = "", symbols: Set[(Char, (Int, Int))] = Set()): (String, Set[(Char, (Int, Int))]) =
    if col == grid(0).length || !grid(row)(col).isDigit then
      (parsed, symbols)
    else
      val neighbors = mooreNeighborhood.map((x, y) => (row + y, col + x)).filter((row, col) => {
        col >= 0 && col < grid(0).length() && row >= 0 && row < grid.length
      }).filter((row, col) =>
        val c = grid(row)(col)
        !(c.isDigit || c == '.'))

      parseNumber(row, col + 1, grid, s"$parsed${grid(row)(col)}", symbols union neighbors.map((row, col) => (grid(row)(col), (row, col))).toSet)


            




