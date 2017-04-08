package ru.tomtrix.probability

import org.scalatest.{FlatSpec, Matchers}
import ru.tomtrix.probability.Dignity._
import ru.tomtrix.probability.Suit._
import ru.tomtrix.probability.Hand._

import scala.util.Random

/**
 * Тестирование старшинства комбинаций
 */
class DeckTest extends FlatSpec with Matchers {

  "RoyalFlush" should "win" in {
    val a = CardSet(Random shuffle List(Card(_10, Hrt), Card(__J, Hrt), Card(__Q, Hrt), Card(__K, Hrt), Card(__A, Hrt), Card(__K, Clb), Card(__K, Spd)))
    val b = CardSet(Random shuffle List(Card(__A, Hrt), Card(__A, Spd), Card(__A, Dmd), Card(__A, Clb), Card(_08, Hrt), Card(_08, Clb), Card(_08, Spd)))
    val c = CardSet(Random shuffle List(Card(__K, Hrt), Card(__K, Spd), Card(__K, Dmd), Card(__K, Clb), Card(_09, Hrt), Card(_09, Clb), Card(_09, Spd)))
    val d = CardSet(Random shuffle List(Card(_10, Hrt), Card(__J, Hrt), Card(__Q, Hrt), Card(__K, Clb), Card(__A, Hrt), Card(__A, Clb), Card(__A, Spd)))
    a.hand should be (RoyalFlush)
    b.hand should be (Quads)
    c.hand should be (Quads)
    d.hand should be (Straight)
    val players = Random shuffle List(a, b, c, d)
    players.sorted should be (List(d, c, b, a))
  }

  "Quads" should "win" in {
    val a = CardSet(Random shuffle List(Card(_02, Dmd), Card(_02, Hrt), Card(_02, Spd), Card(_02, Clb), Card(__K, Hrt), Card(__K, Clb), Card(__K, Spd)))
    val b = CardSet(Random shuffle List(Card(__A, Hrt), Card(__A, Spd), Card(__A, Dmd), Card(_03, Clb), Card(_03, Spd), Card(_03, Dmd), Card(_07, Clb)))
    val c = CardSet(Random shuffle List(Card(__K, Hrt), Card(__K, Spd), Card(__K, Dmd), Card(__Q, Clb), Card(__Q, Spd), Card(__Q, Dmd), Card(__J, Dmd)))
    val d = CardSet(Random shuffle List(Card(_10, Hrt), Card(__J, Hrt), Card(__Q, Hrt), Card(__K, Clb), Card(__A, Hrt), Card(__A, Clb), Card(__A, Spd)))
    a.hand should be (Quads)
    b.hand should be (FullHouse)
    c.hand should be (FullHouse)
    d.hand should be (Straight)
    val players = Random shuffle List(a, b, c, d)
    players.sorted should be (List(d, c, b, a))
  }

  "Flush" should "win" in {
    val a = CardSet(Random shuffle List(Card(_03, Clb), Card(_05, Clb), Card(_07, Clb), Card(_09, Clb), Card(__J, Clb), Card(__J, Spd), Card(__J, Hrt)))
    val b = CardSet(Random shuffle List(Card(__A, Hrt), Card(__A, Spd), Card(_02, Dmd), Card(_02, Clb), Card(_03, Spd), Card(_07, Clb), Card(_10, Dmd)))
    val c = CardSet(Random shuffle List(Card(__K, Hrt), Card(__K, Spd), Card(__Q, Dmd), Card(__Q, Spd), Card(__J, Clb), Card(__J, Dmd), Card(_10, Dmd)))
    val d = CardSet(Random shuffle List(Card(__K, Spd), Card(__A, Spd), Card(_02, Spd), Card(_03, Spd), Card(_04, Hrt), Card(_06, Hrt), Card(_09, Clb)))
    a.hand should be (Flush)
    b.hand should be (TwoPairs)
    c.hand should be (TwoPairs)
    d.hand should be (High)
    val players = Random shuffle List(a, b, c, d)
    players.sorted should be (List(d, c, b, a))
  }

  "Straight" should "win" in {
    val a = CardSet(Random shuffle List(Card(__A, Dmd), Card(_02, Dmd), Card(_03, Dmd), Card(_04, Dmd), Card(_05, Hrt), Card(__Q, Spd), Card(__Q, Clb)))
    val b = CardSet(Random shuffle List(Card(__A, Hrt), Card(__A, Spd), Card(__A, Dmd), Card(_02, Clb), Card(_03, Spd), Card(_07, Clb), Card(__J, Hrt)))
    val c = CardSet(Random shuffle List(Card(__J, Hrt), Card(__J, Spd), Card(__K, Dmd), Card(_02, Clb), Card(_09, Clb), Card(_04, Dmd), Card(_08, Dmd)))
    val d = CardSet(Random shuffle List(Card(_10, Hrt), Card(_10, Dmd), Card(__Q, Hrt), Card(__K, Clb), Card(__A, Hrt), Card(_06, Hrt), Card(_08, Clb)))
    a.hand should be (Straight)
    b.hand should be (Set)
    c.hand should be (Pair)
    d.hand should be (Pair)
    val players = Random shuffle List(a, b, c, d)
    players.sorted should be (List(d, c, b, a))
  }

}
