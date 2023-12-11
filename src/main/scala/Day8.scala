import util.{Day, Util}

object Day8 extends Day(8):
  override def solve(): Unit =
    val instructions = inputLines.head
    val map = inputLines.drop(2)
    val p = raw"^(.+) = \((.+), (.+)\)".r
    val parsed = map.map({case p(from, l, r) => from -> (l, r)}).toMap

    //Part 1
    println(countSteps(parsed, instructions, "AAA", _ == "ZZZ"));

    val starts = parsed.keys.filter(_.endsWith("A"))
    val periods = starts.map(start => countSteps(parsed, instructions, start, _.endsWith("Z")))

    //Part 2
    println(periods.map(_.toLong).reduce(Util.lcm))

  def countSteps(parsed: Map[String, (String, String)], instructions: String, startNode: String, stopCondition: (String => Boolean)): Int =
    LazyList.continually(instructions.toCharArray).flatten.scanLeft((startNode, 1))((acc, c) => {
      if c == 'L' then
        (parsed(acc._1)._1, acc._2 + 1)
      else
        (parsed(acc._1)._2, acc._2 + 1)
    }).takeWhile(v => !stopCondition(v._1)).last._2