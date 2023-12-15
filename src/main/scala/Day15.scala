import util.Day

import scala.annotation.tailrec

object Day15 extends Day(15):
  override def solve(): Unit =
    val instr = input.split(",").toList

    //Part 1
    println(instr.map(hash).sum)

    //Part 2
    println(handleInstructions(instr).map(focusingPower).sum)

  def hash(str: String): Int = str.foldLeft(0)((h, c) => ((h + c.toInt) * 17) % 256)

  @tailrec
  def handleInstructions(instr: List[String], acc: Map[Int, List[(String, Int)]] = Map()): Map[Int, List[(String, Int)]] = instr match
    case x :: xs =>
      if x.endsWith("-") then
        val label = x.substring(0, x.length - 1)
        val box = hash(label)
        handleInstructions(xs, acc.updated(box, acc.getOrElse(box, List()).filter((l, _) => l != label)))
      else
        val s = x.split("=")
        val label = s.head
        val fp = s.last.toInt
        val box = hash(label)
        val list = acc.getOrElse(box, List())

        if list.exists((l, _) => l == label) then
          handleInstructions(xs, acc.updated(box, list.updated(list.indexWhere((l, _) => l == label), (label, fp))))
        else
          handleInstructions(xs, acc.updated(box, list.appended((label, fp))))
    case _ => acc

  def focusingPower(lensBox: (Int, List[(String, Int)])): Int =
    lensBox._2.zipWithIndex.map((l, i) => (1 + lensBox._1)*(i + 1)*l._2).sum