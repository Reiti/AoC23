import util.Day

import scala.annotation.tailrec

object Day5 extends Day(5):
  override def solve(): Unit =
    val split = input.split("\\n\\n").map(_.split("\n")).toList
    val seedRegex = raw"(\d+)".r

    val seeds = seedRegex.findAllMatchIn(split.head(0)).map(r => r.group(1).toLong).toList
    val maps = split.tail.map(parseMap)

    //Part 1
    println(seeds.map(s => mapSeedToLocation(s, maps)).min)

    val seedRanges = seeds.grouped(2).map(l => (l.head, l.head + l(1))).toList

    //Part 2
    println(mapPartitions(seedRanges, maps).minBy(_._1)._1)

  def parseMap(lines: Array[String]): List[(Long, Long, Long)] =
    val regex = raw"(\d+) (\d+) (\d+)".r
    lines.tail.map(line => {
      regex.findFirstMatchIn(line).map(m => (m.group(1).toLong, m.group(2).toLong, m.group(3).toLong)).getOrElse((0L, 0L, 0L))
    }).toList

  @tailrec
  def mapSeedToLocation(pos: Long, maps: List[List[(Long, Long, Long)]]): Long = maps match
    case x :: xs => mapSeedToLocation(findNext(pos, x), xs)
    case _ => pos

  @tailrec
  def findNext(pos: Long, map: List[(Long, Long, Long)]): Long = map match
    case (dest, source, range) :: xs =>
      if withinRange(source, range, pos) then
        val diff = pos - source
        dest + diff
      else
        findNext(pos, xs)
    case _ => pos

  def withinRange(start: Long, range: Long, pos: Long): Boolean = pos >= start && pos < start + range

  @tailrec
  def mapPartitions(partitions: List[(Long, Long)], maps: List[List[(Long, Long, Long)]]): List[(Long, Long)] = maps match
    case x :: xs => mapPartitions(partitions.flatMap(p => mapPartition(p._1, p._2, x)), xs)
    case _ => partitions

  @tailrec
  def mapPartition(seedStart: Long, seedEnd: Long, map: List[(Long, Long, Long)], acc: List[(Long, Long)] = List()): List[(Long, Long)] =
    if seedStart > seedEnd then
      acc
    else
      val (mapped, currRangeEnd) = findRangeEnd(seedStart, seedEnd, map)
      if mapped then
        val s = map.find((dest, source, range) => withinRange(source, range, seedStart)).get
        val mappedStart = s._1 + (seedStart - s._2)
        val mappedEnd = s._1 + (currRangeEnd - s._2)
        mapPartition(currRangeEnd + 1L, seedEnd, map, (mappedStart, mappedEnd) :: acc)
      else
        mapPartition(currRangeEnd + 1L, seedEnd, map, (seedStart, currRangeEnd) :: acc)

  def findRangeEnd(start: Long, seedEnd: Long, map: List[(Long, Long, Long)]): (Boolean, Long) =
    val end = map.find((dest, source, range) => withinRange(source, range, start))

    if end.isDefined then
      val d = end.get
      if seedEnd >= d._2 + d._3 then
        (true, d._2 + d._3 - 1)
      else
        (true, seedEnd)
    else
      val s = map.map((dest, source, range) => (source - start, source)).filter(_._1 > 0)
      if s.isEmpty then
        (false, seedEnd)
      else
        (false, s.minBy(_._1)._2 - 1)