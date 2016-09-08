object Main extends App {

  type Board = Array[Array[Drop]]

  sealed trait Drop
  case object Fire extends Drop {
    override def toString() = "火"
  }
  case object Water extends Drop {
    override def toString() = "水"
  }
  case object Wood extends Drop {
    override def toString() = "木"
  }
  case object Light extends Drop {
    override def toString() = "光"
  }
  case object Dark extends Drop {
    override def toString() = "闇"
  }
  case object Heart extends Drop {
    override def toString() = "♡"
  }
  case object EFire extends Drop
  case object EWater extends Drop
  case object EWood extends Drop
  case object ELight extends Drop
  case object EDark extends Drop
  case object EHeart extends Drop
  case object Dummy extends Drop
  case object Erase extends Drop
  case object Space extends Drop
  val eraseDrops = List(EDark, EFire, EHeart, ELight, EWater, EWood)
  val dropEMap: Map[Drop, Drop] = Map(Fire -> EFire, Water -> EWater, Wood -> EWood, Light -> ELight, Dark -> EDark, Heart -> EHeart)

  sealed trait Direction
  case object Up extends Direction {
    override def toString() = "↑"
  }
  case object Down extends Direction {
    override def toString() = "↓"
  }
  case object Right extends Direction {
    override def toString() = "→"
  }
  case object Left extends Direction {
    override def toString() = "←"
  }
  case object Stop extends Direction {
    override def toString() = "●"
  }
  type Route = List[Direction]

  case class Hand(x: Int, y: Int)

  case class State(board: Board, hand: Hand, route: Route, combo: Int = 0) {
    def startHand: Hand = {
      var (x, y) = (hand.x, hand.y)
      x += route.count(_ == Left)
      x -= route.count(_ == Right)
      y += route.count(_ == Up)
      y -= route.count(_ == Down)
      Hand(x, y)
    }

    override def toString(): String = {
      val sh = startHand
      val josekiSh = (sh.x) + "" + (sh.y + 5)
      // 格ゲー的12346789 が定石メーカーだとこうなる→ 01234789
      val josekiRoute = route.reverse.collect {
        case Up => 6
        case Down => 1
        case Right => 4
        case Left => 3
      } mkString ""
      s"combo:$combo start:(${sh.x + 1},${sh.y + 1}) 手数:${route.size}\troute:${route.reverse.mkString(" ")}\n" +
        s"http://serizawa.web5.jp/puzzdra_theory_maker/index.html?layout=$joseki&route=$josekiSh,$josekiRoute"
    }

  }

  //  val board: Board = Array(
  //    Array(Dark, Water, Dark, Heart, Fire, Fire),
  //    Array(Fire, Light, Dark, Fire, Heart, Heart),
  //    Array(Fire, Dark, Wood, Heart, Wood, Wood),
  //    Array(Water, Heart, Water, Fire, Water, Light),
  //    Array(Light, Heart, Light, Wood, Dark, Light))

  val board: Board = Array.ofDim(5, 6)

  val joseki = readLine()
  for (num <- joseki.toCharArray.toList.zipWithIndex) {
    val n = num._1 - '0'
    val x = num._2 % 6
    val y = num._2 / 6
    val d = n match {
      case 0 => Fire
      case 1 => Wood
      case 2 => Water
      case 3 => Light
      case 4 => Dark
      case 5 => Heart
      case _ => Dummy
    }
    board(y)(x) = d
  }

  println(board.toList.map(_.toList.mkString(" ")).mkString("\n"))
  println()

  val states = getOptimumRoute(board)
  val comboGroup = states.groupBy(_.combo)
  val result = for (g <- comboGroup.values) yield {
    val minMoveState = g.minBy(_.route.size)
    for (s <- g if s == minMoveState || !s.route.mkString("").contains(minMoveState.route.mkString(""))) yield s
  }

  println(result.flatten.toList.sortBy(-_.combo).mkString("\n"))
  //println(states.mkString("\n"))


  def getOptimumRoute(startBoard: Board): List[State] = {
    val firstStates = for (x <- 0 to 5; y <- 0 to 4) yield {
      var buf = scala.collection.mutable.Buffer.empty[State]
      if (x != 0) buf += State(move(board, x, y, Left), Hand(x - 1, y), List(Left))
      if (x != 5) buf += State(move(board, x, y, Right), Hand(x + 1, y), List(Right))
      if (y != 0) buf += State(move(board, x, y, Up), Hand(x, y - 1), List(Up))
      if (y != 4) buf += State(move(board, x, y, Down), Hand(x, y + 1), List(Down))
      buf
    }
    val states = firstStates.flatten.toList

    val firstTop10 = states.map { s =>
      val result = applyBoard(replicateBoard(s.board))
      s.copy(combo = result._2)
    }.sortBy(-_.combo)

    def loop(s: Int, e: Int, top10: List[State]): List[State] = {
      if (s > e) {
        top10.sortBy(x => -(x.combo * 5 - x.route.size)).take(10)
      } else {
        val nexts = (top10 ::: nextStates(top10.filter(_.route.size > s))).sortBy(x => -(x.combo * 5 - x.route.size)).take(5000)
        loop(s + 1, e, nexts)
      }
    }

    loop(0, 60, firstTop10)
  }

  def nextStates(states: List[State]): List[State] = {
    val ret = for (s <- states) yield {
      val buf = scala.collection.mutable.Buffer.empty[State]
      val (x, y, head) = (s.hand.x, s.hand.y, s.route.head)
      if (x != 0 && head != Right) buf += State(move(s.board, x, y, Left), Hand(x - 1, y), Left :: s.route)
      if (x != 5 && head != Left) buf += State(move(s.board, x, y, Right), Hand(x + 1, y), Right :: s.route)
      if (y != 0 && head != Down) buf += State(move(s.board, x, y, Up), Hand(x, y - 1), Up :: s.route)
      if (y != 4 && head != Up) buf += State(move(s.board, x, y, Down), Hand(x, y + 1), Down :: s.route)
      buf
    }
    ret.flatten.map { s =>
      val result = applyBoard(replicateBoard(s.board))
      s.copy(combo = result._2)
    }
  }

  def move(board: Board, x: Int, y: Int, direction: Direction): Board = {
    val nextBoard = replicateBoard(board)
    val nx = if (direction == Left) x - 1 else if (direction == Right) x + 1 else x
    val ny = if (direction == Up) y - 1 else if (direction == Down) y + 1 else y
    nextBoard(y)(x) = board(ny)(nx)
    nextBoard(ny)(nx) = board(y)(x)
    nextBoard
  }

  def replicateBoard(b: Board): Board = {
    val ret: Board = Array.ofDim(5, 6)
    for (x <- 0 to 5; y <- 0 to 4) ret(y)(x) = b(y)(x)
    ret
  }

  def applyBoard(board: Board): (Board, Int) = {
    var totalCombo = 0

    def oneStep(): Int = {
      val nextBoard: Board = Array.ofDim(5, 6)
      var chain = 1
      var nowDrop: Drop = Dummy
      var eraseType: Drop = Dummy
      var combo = 0

      // phase1: search X axis
      for (y <- 0 until 5) {
        chain = 1
        nowDrop = Dummy
        for (x <- 0 until 6) {
          if (board(y)(x) != Space && nowDrop == board(y)(x)) {
            chain += 1
          } else if (chain >= 3) {
            eraseType = dropEMap(board(y)(x - chain))
            while (chain > 0) {
              nextBoard(y)(x - chain) = eraseType
              chain -= 1
            }
            chain = 1
          } else {
            chain = 1
          }
          nowDrop = board(y)(x)
        }
        if (chain >= 3) {
          eraseType = dropEMap(board(y)(6 - chain))
          while (chain > 0) {
            nextBoard(y)(6 - chain) = eraseType
            chain -= 1
          }
        }
      }

      // phase2: search Y axis
      for (x <- 0 until 6) {
        chain = 1
        nowDrop = Dummy
        for (y <- 0 until 5) {
          if (board(y)(x) != Space && nowDrop == board(y)(x)) {
            chain += 1
          } else if (chain >= 3) {
            eraseType = dropEMap(board(y - chain)(x))
            while (chain > 0) {
              nextBoard(y - chain)(x) = eraseType
              chain -= 1
            }
            chain = 1
          } else {
            chain = 1
          }
          nowDrop = board(y)(x)
        }
        if (chain >= 3) {
          eraseType = dropEMap(board(5 - chain)(x))
          while (chain > 0) {
            nextBoard(5 - chain)(x) = eraseType
            chain -= 1
          }
        }
      }

      //printBoard(nextBoard) // for Debug

      // phase3: erase drop
      for (x <- 0 to 5; y <- 0 to 4) {
        if (isEraseDrop(nextBoard(y)(x))) {
          erase(x, y, nextBoard(y)(x))
          combo += 1
        }
      }
      def erase(x: Int, y: Int, d: Drop): Unit = {
        if (nextBoard(y)(x) != d) return
        nextBoard(y)(x) = Erase
        for (i <- -1 to 1; j <- -1 to 1 if 0 <= x + i && x + i < 6 && 0 <= y + j && y + j < 5) {
          erase(x + i, y + j, d)
        }
      }

      // phase4: actually erase drop
      for (x <- 0 to 5; y <- 0 to 4 if nextBoard(y)(x) == Erase) board(y)(x) = Space

      //printBoard(nextBoard) // for Debug
      combo
    }

    def arrangeBoard(): Unit = {
      for (x <- 0 to 5; y <- 1 to 4) {
        if (board(y)(x) == Space) {
          for (yy <- (0 until y).reverse) {
            board(yy + 1)(x) = board(yy)(x)
            board(yy)(x) = Space
          }
        }
      }
    }

    var combo = 0
    do {
      if (totalCombo != 0) arrangeBoard()
      combo = oneStep()
      totalCombo += combo
    } while (combo != 0)

    (board, totalCombo)
  }

  def isEraseDrop(d: Drop): Boolean = eraseDrops.contains(d)

  def printBoard(board: Board): Unit = {
    println(board.map(_.toList).mkString("\n"))
    println()
  }

}


//case object Erase extends Drop
