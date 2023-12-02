import util.Day

object Day2 extends Day(2):
  override def solve(): Unit =
    val sets = inputLines.map(l => {
      val s = l.split(":")
      (s(1).split(";").map(parseSet), s(0).substring("Game ".length()).toInt)
    })

    // Part 1
    println(
      sets
        .filter((e, i) => e.forall(d => d._1 <= 12 && d._2 <= 13 && d._3 <= 14))
        .map(_._2)
        .sum
    )

    // Part 2
    println(
      sets
        .map((e, i) => ((e.maxBy(_._1)._1, e.maxBy(_._2)._2, e.maxBy(_._3)._3)))
        .map(e => e._1 * e._2 * e._3)
        .sum
    )

  def parseSet(set: String): (Int, Int, Int) =
    val blue = raw"(\d*) blue".r
    val red = raw"(\d*) red".r
    val green = raw"(\d*) green".r

    (
      red.findFirstMatchIn(set).map(_.group(1).toInt).getOrElse(0),
      green.findFirstMatchIn(set).map(_.group(1).toInt).getOrElse(0),
      blue.findFirstMatchIn(set).map(_.group(1).toInt).getOrElse(0)
    )
