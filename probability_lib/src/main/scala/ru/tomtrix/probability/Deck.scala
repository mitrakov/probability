package ru.tomtrix.probability

/** Достоинство карты в порядке возрастания */
object Dignity extends Enumeration {val _02, _03, _04, _05, _06, _07, _08, _09, _10, __J, __Q, __K, __A = Value}

/** Масть карты (в алфавитном порядке) */
object Suit extends Enumeration {val Clb, Dmd, Hrt, Spd = Value}

/** Комбинации в порядке большинства */
object Hand extends Enumeration {val None, High, Pair, TwoPairs, Set, Straight, Flush, FullHouse, Quads, StraightFlush, RoyalFlush = Value}

/**
 * Игральная карта
 * @note Сортировка карт реализуется по их достоинству
 * @param dignity - достоинство
 * @param suit - масть
 */
case class Card(dignity: Dignity.Value, suit: Suit.Value) extends Ordered[Card] {
  override def compare(that: Card): Int = dignity.id - that.dignity.id
}

/**
 * Набор карт
 * @note Сортировка набора реализуется по старшинству комбинации (если они равны - то по кикеру)
 * @param cards - список карт (5 или более)
 */
case class CardSet(cards: List[Card]) extends Ordered[CardSet] {
  if (cards.length < 5) throw new IllegalArgumentException("At less 5 cards required")
  lazy val (hand, combination) = getHandAndCombination(cards)

  override def compare(that: CardSet): Int = {
    // сначала сравним "руки": если одна старше другой, то и тот набор старше другого
    val res = hand compare that.hand
    if (res != 0) return res

    // "руки" равны: определим функцию, возвращающую список кикеров (те карты, которые не участвуют в комбинации, в порядке возрастания)
    // обратите внимание: кикерами будут не все карты, а только (5-N), где N - число карт в комбинации, N=2..5
    val kickers = (cs: CardSet) => (cs.cards.toSet -- cs.combination).toList.sorted.takeRight(5 - cs.combination.size)

    // соединим свои кикеры и свою комбинацию в один список, чужие кикеры и чужую комбинацию в др. список
    val pairs = (kickers(this) ++ combination) zip (kickers(that) ++ that.combination)
    // сравним списки попарно в обратном порядке (от старших к младшим, сначала комбинации, затем кикеры)
    for (p <- pairs.reverse) {
      val res = p._1 compare p._2
      if (res != 0) return res
    }

    // комбинации и кикеры равны => наборы равны
    0
  }

  /**
   * Рекурсивная функция вычисления старшей "руки"
   * @param list - список карт (не менее 5)
   * @return значение "руки" и упорядоченный список карт,образующий комбинацию
   * @note для "руки" Старшая Карта список комбинации будет пустым
   * @note возвращаемая комбинация упорядочена не по старшинству достоинства, а по старшинству комбинации; в общем случае
   *       эти два понятия совпадают, однако есть исключения: "A-2-3-4-5" (а не "2-3-4-5-A"), "K-K-7-7-7" (а не "7-7-7-K-K");
   *       таким образом, в случае равенства "рук" две одинаковые комбинации можно смело сравнивать попарно справа налево
   */
  private def getHandAndCombination(list: List[Card]): (Hand.Value, List[Card]) = {
    val lst = list.sorted
    if (lst.size == 5)
      lst match {
        // "10-J-Q-K-A" одной масти => RoyalFlush
        case List(Card(Dignity._10, a), Card(Dignity.__J, b), Card(Dignity.__Q, c), Card(Dignity.__K, d), Card(Dignity.__A, e)) if a==b && b==c && c==d && d==e => Hand.RoyalFlush -> lst
        // все карты идут друг за другом по старшинству + все одной масти => StraightFlush
        case List(Card(u, a), Card(v, b), Card(x, c), Card(y, d), Card(z, e)) if u.id==v.id-1 && v.id==x.id-1 && x.id==y.id-1 && y.id==z.id-1 && a==b && b==c && c==d && d==e => Hand.StraightFlush -> lst
        // особый случай: "2-3-4-5-A" (что есть "A-2-3-4-5") одной масти => StraightFlush
        case List(Card(Dignity._02, a), Card(Dignity._03, b), Card(Dignity._04, c), Card(Dignity._05, d), Card(Dignity.__A, e)) if a==b && b==c && c==d && d==e => Hand.StraightFlush -> (lst.last :: lst.take(4))
        // 4 карты одного достоинства + старшая любая => Quads
        case List(Card(w, _), Card(x, _), Card(y, _), Card(z, _), _) if w==x && x==y && y==z => Hand.Quads -> lst.take(4)
        // 4 карты одного достоинства + младшая любая => Quads
        case List(_,  Card(w, _), Card(x, _), Card(y, _), Card(z, _)) if w==x && x==y && y==z => Hand.Quads -> lst.takeRight(4)
        // 3 младшие карты одинакового достоинства и 2 старшие карты одинакового достоинства => FullHouse
        case List(Card(x, _), Card(y, _), Card(z, _), Card(u, _), Card(v, _)) if x==y && y==z && u==v => Hand.FullHouse -> lst.reverse
        // 2 младшие карты одинакового достоинства и 3 старшие карты одинакового достоинства => FullHouse
        case List(Card(u, _), Card(v, _), Card(x, _), Card(y, _), Card(z, _)) if x==y && y==z && u==v => Hand.FullHouse -> lst
        // 5 карт одинаковой масти => Flush
        case List(Card(_, a), Card(_, b), Card(_, c), Card(_, d), Card(_, e)) if a==b && b==c && c==d && d==e => Hand.Flush -> lst
        // все карты идут друг за другом по старшинству => Straight
        case List(Card(u, _), Card(v, _), Card(x, _), Card(y, _), Card(z, _)) if u.id==v.id-1 && v.id==x.id-1 && x.id==y.id-1 && y.id==z.id-1 => Hand.Straight -> lst
        // особый случай: "2-3-4-5-A" (что есть "A-2-3-4-5") произвольной масти => Straight
        case List(Card(Dignity._02, _), Card(Dignity._03, _), Card(Dignity._04, _), Card(Dignity._05, _), Card(Dignity.__A, _)) => Hand.Straight -> (lst.last :: lst.take(4))
        // 3 карты одного достоинства + 2 старших карты => Set
        case List(Card(x, _), Card(y, _), Card(z, _), _, _) if x==y && y==z => Hand.Set -> lst.take(3)
        // 3 карты одного достоинства + 1 старшая и 1 младшая => Set
        case List(_, Card(x, _), Card(y, _), Card(z, _), _) if x==y && y==z => Hand.Set -> lst.slice(1, 4)
        // 3 карты одного достоинства + 2 младших карты => Set
        case List(_, _, Card(x, _), Card(y, _), Card(z, _)) if x==y && y==z => Hand.Set -> lst.takeRight(3)
        // 2 карты одинакового достоинства + 2 карты одинакового достоинства + старшая карта => TwoPairs
        case List(Card(x, _), Card(y, _), Card(u, _), Card(v, _), _) if x==y && u==v => Hand.TwoPairs -> lst.take(4)
        // 2 карты одинакового достоинства + 2 карты одинакового достоинства + средняя карта => TwoPairs
        case List(Card(x, _), Card(y, _), _, Card(u, _), Card(v, _)) if x==y && u==v => Hand.TwoPairs -> (lst.take(2) ++ lst.takeRight(2))
        // 2 карты одинакового достоинства + 2 карты одинакового достоинства + младшая карта => TwoPairs
        case List(_, Card(x, _), Card(y, _), Card(u, _), Card(v, _)) if x==y && u==v => Hand.TwoPairs -> lst.takeRight(4)
        // 2 карты одного достоинства + 3 старших карты => Pair
        case List(Card(x, _), Card(y, _), _, _, _) if x==y => Hand.Pair -> lst.take(2)
        // 2 карты одного достоинства + 2 старших карты и 1 младшая => Pair
        case List(_, Card(x, _), Card(y, _), _, _) if x==y => Hand.Pair -> lst.slice(1, 3)
        // 2 карты одного достоинства + 1 старшая карта и 2 младших => Pair
        case List(_, _, Card(x, _), Card(y, _), _) if x==y => Hand.Pair -> lst.slice(2, 4)
        // 2 карты одного достоинства + 3 младших карты => Pair
        case List(_, _, _, Card(x, _), Card(y, _)) if x==y => Hand.Pair -> lst.takeRight(2)
        // отсутствие комбинации => High
        case _ => Hand.High -> List.empty
      }
    else if (lst.size > 5) // если карт больше 5 (скажем, 6), то 6 раз изымаем по 1 карте из списка, запускаем рекурсию и выбираем MAX
      lst.map { c => getHandAndCombination(lst filterNot {_==c})} maxBy {_._1}
    else Hand.None -> List.empty
  }
}

/** Колода карт */
object Deck {
  /** Колода карт, 52 листа */
  val pack = List(
    Card(Dignity._02, Suit.Clb), Card(Dignity._02, Suit.Dmd), Card(Dignity._02, Suit.Hrt), Card(Dignity._02, Suit.Spd),
    Card(Dignity._03, Suit.Clb), Card(Dignity._03, Suit.Dmd), Card(Dignity._03, Suit.Hrt), Card(Dignity._03, Suit.Spd),
    Card(Dignity._04, Suit.Clb), Card(Dignity._04, Suit.Dmd), Card(Dignity._04, Suit.Hrt), Card(Dignity._04, Suit.Spd),
    Card(Dignity._05, Suit.Clb), Card(Dignity._05, Suit.Dmd), Card(Dignity._05, Suit.Hrt), Card(Dignity._05, Suit.Spd),
    Card(Dignity._06, Suit.Clb), Card(Dignity._06, Suit.Dmd), Card(Dignity._06, Suit.Hrt), Card(Dignity._06, Suit.Spd),
    Card(Dignity._07, Suit.Clb), Card(Dignity._07, Suit.Dmd), Card(Dignity._07, Suit.Hrt), Card(Dignity._07, Suit.Spd),
    Card(Dignity._08, Suit.Clb), Card(Dignity._08, Suit.Dmd), Card(Dignity._08, Suit.Hrt), Card(Dignity._08, Suit.Spd),
    Card(Dignity._09, Suit.Clb), Card(Dignity._09, Suit.Dmd), Card(Dignity._09, Suit.Hrt), Card(Dignity._09, Suit.Spd),
    Card(Dignity._10, Suit.Clb), Card(Dignity._10, Suit.Dmd), Card(Dignity._10, Suit.Hrt), Card(Dignity._10, Suit.Spd),
    Card(Dignity.__J, Suit.Clb), Card(Dignity.__J, Suit.Dmd), Card(Dignity.__J, Suit.Hrt), Card(Dignity.__J, Suit.Spd),
    Card(Dignity.__Q, Suit.Clb), Card(Dignity.__Q, Suit.Dmd), Card(Dignity.__Q, Suit.Hrt), Card(Dignity.__Q, Suit.Spd),
    Card(Dignity.__K, Suit.Clb), Card(Dignity.__K, Suit.Dmd), Card(Dignity.__K, Suit.Hrt), Card(Dignity.__K, Suit.Spd),
    Card(Dignity.__A, Suit.Clb), Card(Dignity.__A, Suit.Dmd), Card(Dignity.__A, Suit.Hrt), Card(Dignity.__A, Suit.Spd)
  )

  /** индекс карты (преобразует карту в число 0..51), может быть полезно для DB */
  lazy val index = pack.zipWithIndex.map{t => t._1 -> t._2}.toMap
}
