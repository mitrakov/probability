package ru.tomtrix.probability

import org.eclipse.swt.SWT
import org.eclipse.swt.events._
import org.eclipse.swt.graphics.Image
import org.eclipse.swt.layout.{FillLayout, RowData, RowLayout}
import org.eclipse.swt.widgets._
import ru.tomtrix.fvds.ExtList._
import ru.tomtrix.fvds.ExtNumber._
import ru.tomtrix.probability.Deck._
import ru.tomtrix.probability.Forcer._

import scala.slick.driver.SQLiteDriver.simple._
import scala.slick.jdbc.GetResult
import scala.slick.jdbc.StaticQuery.interpolation



/**
 * Object that runs GUI for ready DB (that built with the help of Forcer)
 * @note remove "extends App" if you need the other entry point
 * @author mitrakov (06-2016)
 */
object Gui extends App {
  /** Display */
  val display = Display.getDefault

  /** Ассоциативный массив: карта => картинка карты */
  val cards = (for (card <- pack) yield
  card -> new Image(display, getClass.getResourceAsStream(s"/res/${card.suit}${card.dignity}.bmp"))).toMap

  /** Картинка с "рубашкой" карты */
  val cardBack1 = new Image(display, getClass.getResourceAsStream("/res/back1.bmp"))

  /** Картинка с двумя картами "рубашкой" вверх */
  val cardBack2 = new Image(display, getClass.getResourceAsStream("/res/back2.bmp"))

  /** Картинка с "пустой" картой (чтобы перевернуть выбранную карту обратно) */
  val noneCard = new Image(display, getClass.getResourceAsStream("/res/None.png"))

  /** Картинки мастей */
  val suits = Map(
    Suit.Clb -> new Image(display, getClass.getResourceAsStream("/res/Clb.png")),
    Suit.Dmd -> new Image(display, getClass.getResourceAsStream("/res/Dmd.png")),
    Suit.Hrt -> new Image(display, getClass.getResourceAsStream("/res/Hrt.png")),
    Suit.Spd -> new Image(display, getClass.getResourceAsStream("/res/Spd.png"))
  )

  /** Главная форма */
  val shell = new Shell(display, SWT.SHELL_TRIM)
  shell setText "Probability GUI 3.0.1 (20160611)"
  shell setLayout new FillLayout(SWT.VERTICAL)

  /** Layout for groupEnemies */
  val groupEnemiesLayout = new RowLayout()
  groupEnemiesLayout.wrap = false
  groupEnemiesLayout.justify = true

  /** Groupbox с 7 противниками */
  val groupEnemies = new Group(shell, SWT.SHADOW_ETCHED_OUT)
  groupEnemies setText "Enemies"
  groupEnemies setLayout groupEnemiesLayout
  for (i <- 0 until 7) {
    val layout = new RowLayout(SWT.VERTICAL)
    layout.center = true
    val panel = new Composite(groupEnemies, SWT.NONE)
    panel setLayout layout
    val button = new Button(panel, SWT.NONE)
    button setText "Add enemy"
    button.setLayoutData(new RowData(cardBack2.getBounds.width+12, cardBack2.getBounds.height+12))
    // в качестве данных будет булевый флажок: участвует ли противник в игре
    button setData false
    button addSelectionListener new SelectionAdapter {
      override def widgetSelected(e: SelectionEvent): Unit = {
        // при нажатии кнопки меняем флажок (участвует противник или нет) + изменяем картинку кнопки
        val enemy = button.getData.asInstanceOf[Boolean]
        button setImage (if (enemy) null else cardBack2)
        button setData (!enemy)
        calculate()
      }
    }
    val sp = new Spinner(panel, SWT.BORDER)
    sp setMaximum 1000
    sp addSelectionListener new SelectionAdapter {
      override def widgetSelected(e: SelectionEvent): Unit = calculate()
    }
  }

  /** Функция-обработчик нажатия карты (моих или общих) */
  val listener = (button: Button) => new SelectionAdapter {
    override def widgetSelected(e: SelectionEvent): Unit = {
      chooseCard { card =>
        // пользователь выбрал картинку: запомним карту в поле Data, сменим картинку и вычислим вероятность
        button setData card
        button setImage cards.getOrElse(card, cardBack1)
        calculate()
      }
    }
  }

  /** Layout для groupCommon */
  val groupCommonLayout = new RowLayout()
  groupCommonLayout.wrap = false
  groupCommonLayout.justify = true

  /** Groupbox с 5 общими картами */
  val groupCommon = new Group(shell, SWT.SHADOW_ETCHED_OUT)
  groupCommon setText "Common cards"
  groupCommon setLayout groupCommonLayout
  for (i <- 0 until 5) {
    val button = new Button(groupCommon, SWT.NONE)
    button setImage cardBack1
    button addSelectionListener listener(button)
  }

  /** Layout для groupMine */
  val groupMineLayout = new RowLayout()
  groupMineLayout.wrap = false
  groupMineLayout.justify = true

  /** Groupbox с 2 моими картами и служ. информацией */
  val groupMine = new Group(shell, SWT.SHADOW_ETCHED_OUT)
  groupMine setText "My cards"
  groupMine setLayout groupMineLayout

  /** Левый информационный группбокс */
  val groupInfoLeft = new Composite(groupMine, SWT.NONE)
  groupInfoLeft setLayoutData new RowData(150, 130)
  groupInfoLeft setLayout new RowLayout(SWT.VERTICAL)

  // 2 моих карты
  for (i <- 0 until 2) {
    val button = new Button(groupMine, SWT.NONE)
    button setImage cardBack1
    button addSelectionListener listener(button)
  }

  /** Правый информационный группбокс */
  val groupInfoRight = new Composite(groupMine, SWT.NONE)
  groupInfoRight setLayoutData new RowData(150, 130)
  groupInfoRight setLayout new RowLayout(SWT.VERTICAL)

  /** Кнопка сброса карт и ставок */
  val btnReset = new Button(groupInfoRight, SWT.NONE)
  btnReset setText "Reset"
  btnReset addSelectionListener new SelectionAdapter {
    override def widgetSelected(e: SelectionEvent): Unit = {
      spinnerBlind setSelection 0
      spinnerMine setSelection 0
      val enemyChildren = groupEnemies.getChildren.toList.filter {_.isInstanceOf[Composite]}.cast[Composite].flatMap{_.getChildren}
      enemyChildren.filter{_.isInstanceOf[Spinner]}.cast[Spinner] foreach {_ setSelection 0}
      (groupCommon.getChildren ++ groupMine.getChildren).toList.filter{_.isInstanceOf[Button]}.cast[Button] foreach { button =>
        if (button.getData != null) {
          button setData null
          button setImage cardBack1
        }
      }
      calculate()
    }
  }

  /** Layout для спиннера моих ставок */
  val spinnerMineBox = new Composite(groupInfoLeft, SWT.NONE)
  spinnerMineBox setLayout new FillLayout()
  new Label(spinnerMineBox, SWT.NONE) setText "my bet: "

  /** Спиннер для моих ставок */
  val spinnerMine = new Spinner(spinnerMineBox, SWT.BORDER)
  spinnerMine setMaximum 1000
  spinnerMine addSelectionListener new SelectionAdapter {
    override def widgetSelected(e: SelectionEvent): Unit = calculate()
  }

  /** Layout для спиннера моих блайндов */
  val spinnerBlindBox = new Composite(groupInfoLeft, SWT.NONE)
  spinnerBlindBox setLayout new FillLayout()
  new Label(spinnerBlindBox, SWT.NONE) setText "blind: "

  /** Спиннер для моих блайндов */
  val spinnerBlind = new Spinner(spinnerBlindBox, SWT.BORDER)
  spinnerBlind setMaximum 1000
  spinnerBlind addSelectionListener new SelectionAdapter {
    override def widgetSelected(e: SelectionEvent): Unit = calculate()
  }

  /** Надпись банка */
  val labelPot = new Label(groupInfoLeft, SWT.NONE)
  /** Надпись вероятности */
  val labelProbability = new Label(groupInfoLeft, SWT.NONE)
  /** Надпись total */
  val labelTotal = new Label(groupInfoLeft, SWT.NONE)
  /** Надпись мат. ожидания */
  val labelMath = new Label(groupInfoLeft, SWT.NONE)

  // загрузим класс драйвера JDBC
  Class forName "org.sqlite.JDBC"
  // упаковка формы
  shell.pack()
  // помещаем форму в центр экрана
  shell.setLocation(display.getPrimaryMonitor.getBounds.width/2-shell.getBounds.width/2, display.getPrimaryMonitor.getBounds.height/2-shell.getBounds.height/2)
  // всё готово: открываем форму
  shell.open()
  // main loop
  while (!shell.isDisposed)
    if (!display.readAndDispatch()) display.sleep
  display.dispose()



  // =====================



  /**
   * Вызывает диалог выбора карты: сначала показываем масть, а затем просим выбрать одну из 13 карт выбранной масти
   * @param f - callback, вызываемый при выборе карты
   */
  def chooseCard(f: Card => Unit) = {
    val dialog = new Shell(display, SWT.APPLICATION_MODAL)
    dialog setLayout new RowLayout()
    dialog setAlpha 210

    // функция, которая вызывает суб-диалог для выбора карты заданной масти
    def subShell(suit: Suit.Value) = {
      val sh = new Shell(display, SWT.APPLICATION_MODAL)
      sh setLayout new RowLayout()
      sh setAlpha 210
      for (dignity <- Dignity.values) {
        val button = new Button(sh, SWT.NONE)
        button setImage cards(Card(dignity, suit))
        button pack()
        button addSelectionListener new SelectionAdapter {
          override def widgetSelected(e: SelectionEvent) = {
            // при выборе карты вызываем callback и закрываем все формы (возвразщаемся на главный экран)
            f(Card(dignity, suit))
            sh.close()
            dialog.close()
          }
        }
      }
      sh pack()
      sh setLocation(display.getPrimaryMonitor.getBounds.width/2-sh.getBounds.width/2, display.getPrimaryMonitor.getBounds.height/2-sh.getBounds.height/2)
      sh open()
    }

    // 4 кнопки с мастями
    for (suit <- Suit.values) {
      val button = new Button(dialog, SWT.NONE)
      button setImage suits(suit)
      button pack()
      button addSelectionListener new SelectionAdapter {override def widgetSelected(e: SelectionEvent) = subShell(suit)}
    }

    // доп. кнопка для переворачивания карты рубашкой вверх
    val button = new Button(dialog, SWT.NONE)
    button setImage noneCard
    button pack()
    button addSelectionListener new SelectionAdapter {override def widgetSelected(e: SelectionEvent) = {f(null); dialog.close()}}

    // открываем диалог
    dialog pack()
    dialog setLocation(display.getPrimaryMonitor.getBounds.width/2-dialog.getBounds.width/2, display.getPrimaryMonitor.getBounds.height/2-dialog.getBounds.height/2)
    dialog open()
  }



  /**
   * Запрашивает вероятность из таблиц БД и выводит результат на форму
   */
  def calculate(): Unit = {
    // reset labels
    labelPot setText ""
    labelProbability setText ""
    labelTotal setText ""
    labelMath setText ""

    // вычислим ставку противников
    val enemyChildren = groupEnemies.getChildren.toList.filter {_.isInstanceOf[Composite]}.cast[Composite].flatMap{_.getChildren}
    val enemyBet = enemyChildren.filter{_.isInstanceOf[Spinner]}.cast[Spinner].map{_.getSelection}.sum

    // вычислим мою ставку
    val myBet = spinnerMine.getSelection

    // находим размер чистого выигрыша (ставки противников + мой блайнд)
    val pureWin = enemyBet + spinnerBlind.getSelection

    // находим число игроков (мы + все враги)
    // напомним, что каждая кнопка противника в своём data хранит булевый флажок, участвует игрок в партии или нет
    val players = enemyChildren.count(_.getData.asInstanceOf[Boolean]) + 1 // все враги + мы

    // находим свои карты и общие карты
    // напомним, что каждая кнопка в своём data хранит либо карту, либо null
    val myCards =  groupMine.getChildren.toList.flatMap(t => if (t.getData != null) Some(t.getData.asInstanceOf[Card]) else None).sorted
    val common = groupCommon.getChildren.toList.flatMap(t => if (t.getData != null) Some(t.getData.asInstanceOf[Card]) else None).sorted
    val cards = myCards ++ common

    // опишем класс для доступа к БД
    case class CardResult(success: Int, total: Int)
    implicit val getCardResult = GetResult(r => CardResult(r.<<, r.<<))

    // параметры подключения
    val url = "jdbc:sqlite:probability.db"
    val driver = "scala.slick.driver.SQLiteDriver"

    // функция, которая выводит результаты, полученнные из CardResult, на форму
    val f = (t: CardResult) => {
      println(t)
      val probability = 1.0 * t.success / t.total
      labelPot setText s"Pot = ${myBet + pureWin}"
      labelPot.pack()
      labelProbability setText s"Probability = ${(probability*100).rounded(2)}%"
      labelProbability.pack()
      labelTotal setText s"Total = ${t.total}"
      labelTotal.pack()
      labelMath setText s"Expectancy = ${(pureWin*probability - myBet*(1-probability)).rounded(2)}"
      labelMath.pack()
    }

    // случай, когда открыто 0 карт из прикупа
    if (myCards.length == 2 && common.isEmpty) {
      val suit_range = if (cards.head.suit == cards(1).suit) "0, 1, 3" else "2, 4, 6" // "5" (3-0) is not suitable for both
      Database.forURL(url, driver) withSession { implicit session =>
        sql"""SELECT SUM(success), SUM(total) FROM flop WHERE
            card1 = ${cards.head.dignity.id} AND
            card2 = ${cards(1).dignity.id}   AND
            suit_type IN (#$suit_range)      AND
            players = $players
            GROUP BY card1, card2""".as[CardResult] foreach f
      }
    }

    // случай, когда открыто 3 карты из прикупа
    if (myCards.length == 2 && common.length == 3) {
      val suit_type = getSuitType(cards)
      Database.forURL(url, driver) withSession { implicit session =>
        sql"""SELECT success, total FROM flop WHERE
            card1 = ${cards.head.dignity.id} AND
            card2 = ${cards(1).dignity.id} AND
            card3 = ${cards(2).dignity.id} AND
            card4 = ${cards(3).dignity.id} AND
            card5 = ${cards(4).dignity.id} AND
            suit_type = $suit_type AND
            players = $players""".as[CardResult] foreach f
      }
    }

    // случай, когда открыто 4 карты из прикупа
    if (myCards.length == 2 && common.length == 4) {
      val suit_type = getSuitType(cards)
      Database.forURL(url, driver) withSession { implicit session =>
        sql"""SELECT success, total FROM turn WHERE
            card1 = ${cards.head.dignity.id} AND
            card2 = ${cards(1).dignity.id} AND
            card3 = ${cards(2).dignity.id} AND
            card4 = ${cards(3).dignity.id} AND
            card5 = ${cards(4).dignity.id} AND
            card6 = ${cards(5).dignity.id} AND
            suit_type = $suit_type AND
            players = $players""".as[CardResult] foreach f
      }
    }

    // случай, когда открыто 5 карт из прикупа
    if (myCards.length == 2 && common.length == 5) {
      val suit_type = getSuitType(cards)
      Database.forURL(url, driver) withSession { implicit session =>
        sql"""SELECT success, total FROM river WHERE
            card1 = ${cards.head.dignity.id} AND
            card2 = ${cards(1).dignity.id} AND
            card3 = ${cards(2).dignity.id} AND
            card4 = ${cards(3).dignity.id} AND
            card5 = ${cards(4).dignity.id} AND
            card6 = ${cards(5).dignity.id} AND
            card7 = ${cards(6).dignity.id} AND
            suit_type = $suit_type AND
            players = $players""".as[CardResult] foreach f
      }
    }
  }
}
