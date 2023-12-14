import util.Day

object Day13 extends Day(13):
  override def solve(): Unit =
    val notes = input.split("\n\n").map(_.split("\n"))

    val scores = notes.map(n => findReflection(n.toList))

    //Part 1
    println(scores.sum)

    val oldScores = notes.zip(scores)

    //Part 2
    println(oldScores.map(findWithSmudges).sum)


  def findReflection(note: List[String]): Int =
    val horizontal = find(note).maxBy(_._2)
    if horizontal._2 == 0 then
      find(note.transpose.map(_.mkString(""))).maxBy(_._2)._1
    else
      100 * horizontal._1

  def find(note: List[String]): List[(Int, Int)] = note.indices.map(i =>
    val (l, r) = note.splitAt(i)
    val ref = l.reverse.zip(r).takeWhile(z => z._1 == z._2).length
    if ref == Math.min(l.length, r.length) then (i, ref) else (i, 0)
  ).toList

  def findWithSmudges(note: (Array[String], Int)): Int =
    val unsmudged = note._1.indices.flatMap(o => {
      note._1(o).indices.map(i => {
        note._1.updated(o, note._1(o).updated(i, if note._1(o)(i) == '#' then '.' else '#'))
      })
    })
    val refH = unsmudged.flatMap(s => find(s.toList)).filter(_._2 != 0).map(_._1 * 100)
    val refV = unsmudged.flatMap(s => find(s.toList.transpose.map(_.mkString))).filter(_._2 != 0).map(_._1)

    (refH ++ refV).find(v => v != note._2).get
