package ru.tomtrix.probability

import ru.tomtrix.fvds.ExtString._
import ru.tomtrix.probability.Deck._

import scala.compat.Platform
import scala.slick.driver.SQLiteDriver.simple._
import scala.slick.jdbc.StaticQuery.interpolation
import scala.util.Random

//import ru.tomtrix.fvds.ExtString._
//import ru.tomtrix.probability.Deck._
//import scala.compat.Platform
//import scala.slick.driver.SQLiteDriver.simple._
//import scala.slick.jdbc.StaticQuery.interpolation
//import scala.util.Random



/**
 * Object that forces the combinations and keeps the results in DB
 * @note remove "extends App" if you need the other entry point
 * @author mitrakov (06-2016)
 */
object Forcer extends App {
  val rand = new Random(Platform.currentTime)
  val start = Platform.currentTime
  args match {
    case Array(host, players, dbCards, n) if players.toIntOpt.isDefined && dbCards.toIntOpt.isDefined && n.toIntOpt.isDefined =>
      println(s"Start forcing $n samples for $players players (saving $dbCards cards to DB)")
      for (i <- 0 until n.toInt) run(host, players.toInt, dbCards.toInt)
      println(s"Done... (${(Platform.currentTime - start)/1000} sec)")
    case _ => println("Usage java -jar flop_table.jar <host> <players> <count of cards to save to DB> <count of samples>\nExample: \"java -jar flop_table.jar 127.0.0.1 4 5 1000000\" forces a million samples with 4 players and saves data on a \"flop\" stage (5 cards) to DB which is run on localhost")
  }



  /**
   * Simulates a single game
   * @param host - server that hosts a DB
   * @param playerCnt - count of players (2-8)
   * @param dbCardsCnt - count of cards to keep in DB (5 for 'flop', 6 for 'turn', 7 for 'river')
   */
  def run(host: String, playerCnt: Int, dbCardsCnt: Int): Unit = {
    val shuffledPack = rand shuffle pack
    val commonCards = shuffledPack takeRight 5
    val players = for (i <- 0 until playerCnt) yield {
      val twoCards = shuffledPack.slice(2*i, 2*(i+1))
      CardSet(twoCards ++ commonCards)
    }
    val me = players.head

    // сохраняем в БД
    val url = s"jdbc:mysql://$host:3306/probability?useSSL=false"
    Database.forURL(url, "mitrakov", "541888", null, "com.mysql.jdbc.Driver").withSession { implicit session =>
      val success =  if (players.max == me) 1 else 0
      val list1 = me.cards.take(2).sorted
      val list2 = me.cards.slice(2, dbCardsCnt).sorted
      val suit_type = getSuitType(list1 ++ list2) // don't use "suit_type = getSuitType(me.cards)"

      // запись в таблицу в зав-ти от числа сохраняемых карт
      dbCardsCnt match {
        case 5 =>
          sqlu"""INSERT INTO `flop` (`card1`, `card2`, `card3`, `card4`, `card5`, `suit_type`, `players`, `success`, `total`)
          VALUES (${list1.head.dignity.id}, ${list1(1).dignity.id}, ${list2.head.dignity.id}, ${list2(1).dignity.id}, ${list2(2).dignity.id}, $suit_type, $playerCnt, $success, 1)
          ON DUPLICATE KEY UPDATE `success` = `success` + $success, `total` = `total` + 1""".execute
        case 6 =>
          sqlu"""INSERT INTO `turn` (`card1`, `card2`, `card3`, `card4`, `card5`, `card6`, `suit_type`, `players`, `success`, `total`)
          VALUES (${list1.head.dignity.id}, ${list1(1).dignity.id}, ${list2.head.dignity.id}, ${list2(1).dignity.id}, ${list2(2).dignity.id}, ${list2(3).dignity.id}, $suit_type, $playerCnt, $success, 1)
          ON DUPLICATE KEY UPDATE `success` = `success` + $success, `total` = `total` + 1""".execute
        case 7 =>
          sqlu"""INSERT INTO `river` (`card1`, `card2`, `card3`, `card4`, `card5`, `card6`, `card7`, `suit_type`, `players`, `success`, `total`)
          VALUES (${list1.head.dignity.id}, ${list1(1).dignity.id}, ${list2.head.dignity.id}, ${list2(1).dignity.id}, ${list2(2).dignity.id}, ${list2(3).dignity.id}, ${list2(4).dignity.id}, $suit_type, $playerCnt, $success, 1)
          ON DUPLICATE KEY UPDATE `success` = `success` + $success, `total` = `total` + 1""".execute
        case _ =>
      }
    }
  }



  /**
   * @param cards - list of cards (5-7)
   * @return suit combination type to store in DB
   */
  def getSuitType(cards: List[Card]): Int = {
    // возвращает true, если масти карт mine/others соответствуют типу комбинации "x-y"
    // например, 4-2 означает, что всего 4 карты лидирующей масти, из них 2 - мои. Такая комбинация имеет тип 1
    def w[T](mine: T*)(other: T*)(x: Int, y: Int): Boolean = {
      val group: Map[T, Seq[T]] = (mine ++ other) groupBy {t => t}
      val maxLen = group.map{_._2.length}.max

      val combs: List[(T, Seq[T])] = group.filter{_._2.length == maxLen}.toList
      val bestT = combs.length match {
        case 1 => combs.head._1
        case _ => combs.find{t => !mine.contains(t._1)} match {
          case Some(z) => z._1
          case None => combs.head._1
        }
      }
      val myList = mine filter {_ == bestT}
      maxLen == x && myList.length == y
    }

    // рассмотрим случаи 5,6,7 карт (строго соблюдать порядок кейсов!)
    cards match {
      // 5 cards
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e)) if w(a,b)(c,d,e)(5,2) => 0 // 5-2
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e)) if w(a,b)(c,d,e)(4,2) => 1 // 4-2
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e)) if w(a,b)(c,d,e)(4,1) => 2 // 4-1
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e)) if w(a,b)(c,d,e)(3,2) => 3 // 3-2
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e)) if w(a,b)(c,d,e)(3,1) => 4 // 3-1
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e)) if w(a,b)(c,d,e)(3,0) => 5 // 3-0

      // 6 cards
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f)) if w(a,b)(c,d,e,f)(6,2) => 7 // 6-2
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f)) if w(a,b)(c,d,e,f)(5,2) => 0 // 5-2
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f)) if w(a,b)(c,d,e,f)(5,1) => 8 // 5-1
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f)) if w(a,b)(c,d,e,f)(4,2) => 1 // 4-2
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f)) if w(a,b)(c,d,e,f)(4,1) => 2 // 4-1
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f)) if w(a,b)(c,d,e,f)(4,0) => 9 // 4-0
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f)) if w(a,b)(c,d,e,f)(3,1) => 4 // 3-1
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f)) if w(a,b)(c,d,e,f)(3,0) => 5 // 3-0

      // 7 cards
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f), Card(_, g)) if w(a,b)(c,d,e,f,g)(7,2) => 10 // 7-2
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f), Card(_, g)) if w(a,b)(c,d,e,f,g)(6,2) => 7  // 6-2
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f), Card(_, g)) if w(a,b)(c,d,e,f,g)(6,1) => 11 // 6-1
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f), Card(_, g)) if w(a,b)(c,d,e,f,g)(5,2) => 0  // 5-2
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f), Card(_, g)) if w(a,b)(c,d,e,f,g)(5,1) => 8  // 5-1
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f), Card(_, g)) if w(a,b)(c,d,e,f,g)(5,0) => 12 // 5-0
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f), Card(_, g)) if w(a,b)(c,d,e,f,g)(4,2) => 13 // 4-x
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f), Card(_, g)) if w(a,b)(c,d,e,f,g)(4,1) => 13 // 4-x
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f), Card(_, g)) if w(a,b)(c,d,e,f,g)(4,0) => 13 // 4-x
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f), Card(_, g)) if w(a,b)(c,d,e,f,g)(3,2) => 14 // 3-x
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f), Card(_, g)) if w(a,b)(c,d,e,f,g)(3,1) => 14 // 3-x
      case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e), Card(_, f), Card(_, g)) if w(a,b)(c,d,e,f,g)(3,0) => 14 // 3-x

      case _ => 6 // others
    }
  }
}


//installing MySQL for Fedora Server 23:
//
//dnf install mysql-server
//service mysql start
//mysqladmin -u root password NEWPASSWORD
//mysql -uroot -p
//CREATE USER 'mitrakov'@'%' IDENTIFIED BY '12345';
//CREATE USER 'mitrakov'@'localhost' IDENTIFIED BY '12345';
//CREATE DATABASE probability;
//GRANT ALL PRIVILEGES ON probability.* TO 'mitrakov'@'%' IDENTIFIED BY '12345' WITH GRANT OPTION;
//iptables -I INPUT 1 -p tcp --dport 3306 -j ACCEPT

//CREATE TABLE `flop` (
//`flop_id` BIGINT(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'primary_key',
//`card1` TINYINT(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT 'my first card',
//`card2` TINYINT(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT 'my second card',
//`card3` TINYINT(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT 'common flop card #1',
//`card4` TINYINT(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT 'common flop card #2',
//`card5` TINYINT(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT 'common flop card #3',
//`suit_type` TINYINT(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT '0 is 5-2, 1 is 4-2, 2 is 4-1, 3 is 3-2, 4 is 3-1, 5 is 3-0, 6 is other',
//`players` TINYINT(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT 'count of players',
//`success` SMALLINT(5) UNSIGNED NOT NULL DEFAULT '0' COMMENT 'count of success plays',
//`total` SMALLINT(5) UNSIGNED NOT NULL DEFAULT '0' COMMENT 'count of plays',
//PRIMARY KEY (`flop_id`),
//UNIQUE INDEX `combination` (`card1`, `card2`, `card3`, `card4`, `card5`, `suit_type`, `players`)
//)
//COMMENT='flop table'
//COLLATE='utf8_bin'
//ENGINE=InnoDB
//;

//CREATE TABLE `turn` (
//`turn_id` BIGINT UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'primary_key',
//`card1` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'my first card',
//`card2` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'my second card',
//`card3` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'common flop card #1',
//`card4` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'common flop card #2',
//`card5` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'common flop card #3',
//`card6` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'common turn card',
//`suit_type` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT '4 is 3-1, 5 is 3-0, 6 is other, 7 is 6-2, 8 is 5-2, 9 is 5-1, 10 is 4-2, 11 is 4-1, 12 is 4-0',
//`players` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'count of players',
//`success` SMALLINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'count of success plays',
//`total` SMALLINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'count of plays',
//PRIMARY KEY (`turn_id`),
//UNIQUE INDEX `combination` (`card1`, `card2`, `card3`, `card4`, `card5`, `card6`, `suit_type`, `players`)
//)
//COMMENT='turn table'
//COLLATE='utf8_bin'
//ENGINE=InnoDB
//;


//CREATE TABLE `river` (
//`river_id` BIGINT UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'primary key',
//`card1` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'my first card',
//`card2` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'my second card',
//`card3` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'common flop card #1',
//`card4` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'common flop card #2',
//`card5` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'common flop card #3',
//`card6` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'common turn card',
//`card7` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'common river card',
//`suit_type` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT '10 is 7-2, 7 is 6-2, 11 is 6-1, 0 is 5-2, 8 is 5-1, 12 is 5-0, 6 is others',
//`players` TINYINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'count of players',
//`success` SMALLINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'count of success plays',
//`total` SMALLINT UNSIGNED NOT NULL DEFAULT '0' COMMENT 'count of plays',
//PRIMARY KEY (`river_id`),
//UNIQUE INDEX `combination` (`card1`, `card2`, `card3`, `card4`, `card5`, `card6`, `card7`, `suit_type`, `players`)
//)
//COMMENT='river table'
//COLLATE='utf8_bin'
//ENGINE=InnoDB
//;
