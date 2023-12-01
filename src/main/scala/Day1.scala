import util.Day

import scala.annotation.tailrec

object Day1 extends Day(1):
  override def solve(): Unit =
    //Part 1
    println(inputLines.map(l => l.filter(_.isDigit)).map(l => Integer.parseInt(s"${l.head}${l.last}")).sum)

    //Part 2
    println(inputLines.map(l => Integer.parseInt(s"${first(l)}${last(l)}")).sum)

    @tailrec
    def first(in: String, acc: String = ""): Int = acc match
        case s if s.endsWith("one") => 1
        case s if s.endsWith("two") => 2
        case s if s.endsWith("three") => 3
        case s if s.endsWith("four") => 4
        case s if s.endsWith("five") => 5
        case s if s.endsWith("six") => 6
        case s if s.endsWith("seven") => 7
        case s if s.endsWith("eight") => 8
        case s if s.endsWith("nine") => 9
        case _ =>
          if in.head.isDigit then
            in.head.asDigit
          else
            first(in.tail, acc + in.head)

    @tailrec
    def last(in: String, acc: String = ""): Int = acc match
        case s if s.startsWith("one") => 1
        case s if s.startsWith("two") => 2
        case s if s.startsWith("three") => 3
        case s if s.startsWith("four") => 4
        case s if s.startsWith("five") => 5
        case s if s.startsWith("six") => 6
        case s if s.startsWith("seven") => 7
        case s if s.startsWith("eight") => 8
        case s if s.startsWith("nine") => 9
        case _ =>
          if in.reverse.head.isDigit then
            in.reverse.head.asDigit
          else
            last(in.reverse.tail.reverse, s"${in.reverse.head}$acc")