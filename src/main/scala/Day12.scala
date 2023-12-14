import util.{Day, Util}

object Day12 extends Day(12):
  override def solve(): Unit =
    val regex = raw"^(.+) (.+)".r

    val rows = inputLines.map({ case regex(line, nums) => Row(line, nums.split(",").map(_.toInt).toList) })

    //Part 1
    println(rows.map({ case Row(springs, broken) =>
      countPossibilities(springs, broken, broken.sum, 0, ' ')
    }
    ).sum)

    //Part 2
    println(rows.map(expand).map({ case Row(springs, broken) =>
      countPossibilities(springs, broken, broken.sum, 0, ' ')
    }
    ).sum)

  case class Row(springs: String, broken: List[Int])

  def expand(row: Row): Row = Row((row.springs + "?").repeat(5).reverse.tail.reverse, LazyList.continually(row.broken).take(5).flatten.toList)

  lazy val countPossibilities: ((String, List[Int], Int, Int, Char)) => Long = Util.memoize {
    case (springs, broken, sum, count, last) =>
      if count > sum then
        0L
      else
        springs match
          case s"?$xs" =>
            if broken.isEmpty then
              countPossibilities(xs, broken, sum, count, '.')
            else if broken.head == 0 then
              countPossibilities(xs, broken.tail, sum, count, '.')
            else
              val withBroken = countPossibilities(xs, broken.head - 1 :: broken.tail, sum, count + 1, '#')
              val withOutBroken =
                if last == '#' then
                  countPossibilities(xs, broken.tail, sum, count, '.')
                else
                  countPossibilities(xs, broken, sum, count, '.')

              withBroken + withOutBroken
          case s"#$xs" =>
            if broken.isEmpty || (broken.length == 1 && broken.head == 0) then
              0L
            else if broken.head == 0 && last == '#' then
              0L
            else if broken.head == 0 then
              countPossibilities(xs, broken.tail.head - 1 :: broken.tail.tail, sum, count + 1, '#')
            else
              countPossibilities(xs, broken.head - 1 :: broken.tail, sum, count + 1, '#')
          case s".$xs" =>
            if broken.nonEmpty && broken.head == 0 then
              countPossibilities(xs, broken.tail, sum, count, '.')
            else if last == '#' then
              countPossibilities(xs, broken.tail, sum, count, '.')
            else
              countPossibilities(xs, broken, sum, count, '.')
          case "" =>
            if (broken.isEmpty || (broken.length == 1 && broken.head == 0)) && sum == count then
              1L
            else
              0L
  }