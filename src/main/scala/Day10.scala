import util.{Day, Util}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Day10 extends Day(10):
  override def solve(): Unit =
    val inputMapWithDefault = inputMap.withDefault(_ => '.')
    val start = inputMapWithDefault.find(_._2 == 'S').get._1

    val loop = findPath(inputMapWithDefault, start, findDirections(inputMapWithDefault, start).head).toSet

    //Part 1
    println(loop.size / 2)

    val withoutUnconnected = inputMapWithDefault.map((key, value) => if loop.contains(key) then (key, value) else (key, '.'))

    //Part 2
    println(shrink(floodFill(Util.parseMap(expand(withoutUnconnected).toArray), Set((0, 0)))).count((_, c) => c == '.'))

  def findDirections(map: Map[(Int, Int), Char], start: (Int, Int)): List[(Int, Int)] =
    val up = map((start._1 - 1, start._2))
    val down = map((start._1 + 1, start._2 ))
    val left = map((start._1, start._2 - 1))
    val right = map((start._1, start._2 + 1))

    val resultList = ArrayBuffer[(Int, Int)]()

    if up == '|' || up == '7' || up == 'F' then
      resultList.append((-1, 0))

    if down == '|' || down == 'L' || down == 'J' then
      resultList.append((1, 0))

    if left == '-' || left == 'L' || left == 'F' then
      resultList.append((0, -1))

    if right == '-' || right == '7' || right == 'J' then
      resultList.append((0, 1))

    resultList.toList

  @tailrec
  def findPath(map: Map[(Int, Int), Char], pos: (Int, Int), dir: (Int, Int), path: List[(Int, Int)] = List()): List[(Int, Int)] =
    val nextPos = (pos._1 + dir._1, pos._2 + dir._2)
    val next = map(nextPos)

    if next == 'S' then
     pos :: path
    else
      val nextDir = next match
        case 'L' => (dir._2, dir._1)
        case 'J' => ((-1) * dir._2, (-1) * dir._1)
        case '7' => (dir._2, dir._1)
        case 'F' => ((-1) * dir._2, (-1) * dir._1)
        case _ => dir
      findPath(map, nextPos, nextDir, pos :: path)

  def expand(map: Map[(Int, Int), Char]): List[String] =
    (0 to map.keys.maxBy(_._1)._1).flatMap(x => {
      (0 to map.keys.maxBy(_._2)._2).map(y => {
        map(x, y) match
          case 'L' => List(
            ".|.",
            ".L-",
            "..."
          )
          case 'J' => List(
            ".|.",
            "-J.",
            "..."
          )
          case '7' => List(
            "...",
            "-7.",
            ".|."
          )
          case 'F' => List(
            "...",
            ".F-",
            ".|."
          )
          case '|' => List(
            ".|.",
            ".|.",
            ".|."
          )
          case '-' => List(
            "...",
            "---",
            "..."
          )
          case '.' => List(
            "...",
            "...",
            "..."
          )
          case 'S' => List(
            ".|.",
            "-S-",
            ".|."
          )
      }).reduce((l, r) => l.zip(r).map((a, b) => a + b)).toList
    }).toList

  @tailrec
  def floodFill(map: Map[(Int, Int), Char], locs: Set[(Int, Int)]): Map[(Int, Int), Char] =
    val next = locs.flatMap(loc => Util.vonNeumannNeighborhood.map(n => {
      (loc._1 + n._1, loc._2 + n._2)
    }).filter(pos => map.contains(pos)).filter(p => map(p) == '.'))

    if next.isEmpty then
      map
    else
      floodFill(map.map((pos, c) => {
        if locs.contains(pos) then (pos, 'O') else (pos, c)
      }), next)

  @tailrec
  def shrink(map: Map[(Int, Int), Char], row: Int = 0, col: Int = 0, acc: Map[(Int, Int), Char] = Map()): Map[(Int, Int), Char] =
    if row >= map.keys.maxBy(_._1)._1 then
      acc
    else if col >= map.keys.maxBy(_._2)._2 then
      shrink(map, row + 3, 0, acc)
    else
      val tile = (row until row + 3).map(row =>
        (col until col + 3).map(col => map((row, col))).toArray
      ).toArray
      shrink(map, row, col + 3, acc.updated((row / 3, col / 3), tile(1)(1)))