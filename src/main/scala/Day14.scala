import util.{Day, Util}

import scala.annotation.tailrec

object Day14 extends Day(14):
  override def solve(): Unit =
    val inputMapWithDefault = inputMap.withDefault(_ => '#')

    //Part 1
    println(score(tilt(inputMapWithDefault, (-1, 0))))

    val (a, b, results) = findCycle(inputMapWithDefault)
    val cycleLength = b - a
    val remainder = (1000000000 - a) % cycleLength

    //Part 2
    println(score(results.find(_._2 == (a + remainder)).get._1))

  @tailrec
  def findCycle(map: Map[(Int, Int), Char], iteration: Int = 0, previous: Map[Map[(Int, Int), Char], Int] = Map()): (Int, Int, Map[Map[(Int, Int), Char], Int]) =
    if previous.contains(map) then
      (previous(map), iteration, previous)
    else
      findCycle(cycle(map), iteration + 1, previous + (map -> iteration))

  def cycle(map: Map[(Int, Int), Char]): Map[(Int, Int), Char] =
    tilt(tilt(tilt(tilt(map, (-1, 0)), (0, -1)), (1, 0)), (0, 1))

  @tailrec
  def tilt(map: Map[(Int, Int), Char], direction: (Int, Int)): Map[(Int, Int), Char] = map.find((coords, char) => {
    char == 'O' && map(coords._1 + direction._1, coords._2 + direction._2) == '.'
  }) match
    case Some(entry) =>
      tilt(map.updated(entry._1, '.').updated((entry._1._1 + direction._1, entry._1._2 + direction._2), 'O'), direction)
    case None => map

  def score(map: Map[(Int, Int), Char]): Int =
    val height = map.keys.maxBy(_._1)._1

    map.filter(_._2 == 'O').map(entry => height - entry._1._1 + 1).sum

  def printMap(map: Map[(Int, Int), Char]): Unit =
    (0 to map.keys.maxBy(_._1)._1).foreach(row => {
      (0 to map.keys.maxBy(_._2)._2).foreach(col => {
        print(map(row, col))
      })
      println
    })