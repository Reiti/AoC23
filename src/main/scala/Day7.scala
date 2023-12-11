import util.Day

import scala.annotation.tailrec

object Day7 extends Day(7):
  override def solve(): Unit =
    val hands = inputLines.map(parse)

    val sorted = hands.sortWith((l, r) => {
      if l.strength == r.strength then
        higher(l.cards, r.cards, value)
      else
        l.strength < r.strength
    }).reverse.zipWithIndex.map(z => (z._1, z._2 + 1))

    //Part 1
    println(sorted.map(z => z._1.bid * z._2).sum)

    val sortedJ = hands.sortWith((l, r) => {
      val ls = strengthWithJokers(l.cards)
      val rs = strengthWithJokers(r.cards)
      if ls == rs then
        higher(l.cards, r.cards, valueJoker)
      else
        ls < rs
    }).reverse.zipWithIndex.map(z => (z._1, z._2 + 1))

    //Part 2
    println(sortedJ.map(z => z._1.bid * z._2).sum)

  case class Hand(cards: String, bid: Int, strength: Int)

  def buildMap(l: List[Char]): List[List[Char]] =
    l.flatMap(v => l.flatMap(w => l.flatMap(x => l.flatMap(y => l.map(z => List(v, w, x, y, z)))))).distinct

  def parse(hand: String): Hand =
    val s = hand.split(" ");
    val cards = s(0)
    val bid = s(1).toInt
    Hand(cards, bid, calcStrength(cards))


  def calcStrength(cards: String): Int =
    val grouped = cards.groupBy(identity).view.mapValues(_.length).toMap

    if grouped.size == 1 then
      0
    else if grouped.size == 2 then
      if grouped.exists(_._2 == 4) then
        1
      else
        2
    else if grouped.size == 3 then
      if grouped.exists(_._2 == 3) then
        3
      else
        4
    else if grouped.size == 4 then
      5
    else
      6

  @tailrec
  def higher(l: String, r: String, v: (Char => Int)): Boolean =
    if v(r.head) > v(l.head) then
      false
    else if v(l.head) > v(r.head) then
      true
    else
      higher(l.tail, r.tail, v)

  def value(card: Char): Int = card match
    case 'A' => 14
    case 'K' => 13
    case 'Q' => 12
    case 'J' => 11
    case 'T' => 10
    case _ => card.asDigit

  def valueJoker(card: Char): Int = card match
    case 'A' => 14
    case 'K' => 13
    case 'Q' => 12
    case 'J' => 1
    case 'T' => 10
    case _ => card.asDigit

  def strengthWithJokers(cards: String): Int =
    val possibleCards = buildMap(if cards.count(_ == 'J') == 5 then List('A') else cards.distinct.filter(_ != 'J').toList)
    val jokerCount = cards.count(_ == 'J')
    possibleCards.map(p => replace(cards, p.take(jokerCount))).map(c => calcStrength(c)).min

  @tailrec
  def replace(cards: String, values: List[Char]): String = values match
    case x :: xs => replace(cards.replaceFirst("J", s"$x"), xs)
    case _ =>
      cards