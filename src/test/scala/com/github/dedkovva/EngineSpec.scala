package com.github.dedkovva

import org.scalatest.{FreeSpec, Matchers}
import Types._
import Types.Pieces._

import scala.collection.immutable.SortedSet

class EngineSpec extends FreeSpec with Matchers {
  import Engine._

  "rotate spec" in {
    val mn = MxN(4, 4)

    import scala.language.implicitConversions
    implicit def boardEntriesToBoard(boardEntries: BoardEntries): Board = Board(mn, boardEntries)

    val e0 = SortedSet(
      BoardEntry(Position(0, 0), Q),
      BoardEntry(Position(2, 1), Q),
      BoardEntry(Position(1, 3), B))
    val e0Str =
      """||Q| | | |
         || | | |B|
         || |Q| | |
         || | | | |""".stripMargin
    boardToASCII(e0) shouldBe e0Str

    val e1 = rotateSquareBoardCounterWise(e0)
    e1 shouldBe SortedSet(
      BoardEntry(Position(3, 0), Q),
      BoardEntry(Position(2, 2), Q),
      BoardEntry(Position(0, 1), B))
    boardToASCII(e1) shouldBe
      """|| |B| | |
         || | | | |
         || | |Q| |
         ||Q| | | |""".stripMargin

    val e2 = rotateSquareBoardCounterWise(e1)
    boardToASCII(e2) shouldBe
      """|| | | | |
         || | |Q| |
         ||B| | | |
         || | | |Q|""".stripMargin

    val e3 = rotateSquareBoardCounterWise(e2)
    boardToASCII(e3) shouldBe
      """|| | | |Q|
         || |Q| | |
         || | | | |
         || | |B| |""".stripMargin

    val e4 = rotateSquareBoardCounterWise(e3)
    boardToASCII(e4) shouldBe e0Str

    e0 shouldBe e4
  }

  "is safe positions spec 01" in {
    isSafeStraight(Position(0, 0), Position(0, 0)) shouldBe false
    isSafeStraight(Position(0, 0), Position(0, 9)) shouldBe false
    isSafeStraight(Position(0, 0), Position(1, 9)) shouldBe true
    isSafeStraight(Position(0, 0), Position(9, 0)) shouldBe false

    isSafeDiagonal(Position(0, 0), Position(0, 0)) shouldBe false
    isSafeDiagonal(Position(0, 0), Position(9, 9)) shouldBe false
    isSafeDiagonal(Position(0, 0), Position(9, 8)) shouldBe true

    isSafeKing(Position(0, 0), Position(0, 0)) shouldBe false
    isSafeKing(Position(0, 0), Position(1, 0)) shouldBe false
    isSafeKing(Position(0, 0), Position(1, 1)) shouldBe false
    isSafeKing(Position(0, 0), Position(1, 2)) shouldBe true

    isSafeKnight(Position(0, 0), Position(0, 0)) shouldBe false
    isSafeKnight(Position(0, 0), Position(2, 1)) shouldBe false
    isSafeKnight(Position(0, 0), Position(1, 2)) shouldBe false
    isSafeKnight(Position(0, 0), Position(1, 1)) shouldBe true
    isSafeKnight(Position(0, 0), Position(0, 1)) shouldBe true
    isSafeKnight(Position(0, 0), Position(3, 3)) shouldBe true
  }

  "is safe position spec 02" in {
    import Pieces._
    isSafePosition(BoardEntry(Position(0, 1), Q), Position(3, 5)) shouldBe true
    isSafePosition(BoardEntry(Position(3, 5), B), Position(0, 1)) shouldBe true
    isSafePosition(BoardEntry(Position(0, 1), Q), Position(4, 5)) shouldBe false
    isSafePosition(BoardEntry(Position(4, 5), B), Position(0, 1)) shouldBe false
  }

  "sort piece-count spec" in {
    val seq1 = Seq(
      PieceCount(K, 2),
      PieceCount(Q, 1),
      PieceCount(R, 2)
    )

    sortPieceCountSeq(seq1) shouldBe Seq(
      PieceCount(Q, 1),
      PieceCount(R, 2),
      PieceCount(K, 2)
    )
  }

  "symmetrize boards spec 01" in {
    val mn = MxN(4, 2)
    val boardEntries = SortedSet(
      BoardEntry(Position(0, 0), B),
      BoardEntry(Position(1, 0), N),
      BoardEntry(Position(2, 0), B),
      BoardEntry(Position(3, 0), N))
    val sym = symmetrizeBoard(Board(mn, boardEntries))

    val symStr = Set(
      """||B| |
         ||N| |
         ||B| |
         ||N| |""".stripMargin,

      """||N| |
         ||B| |
         ||N| |
         ||B| |""".stripMargin,

      """|| |B|
         || |N|
         || |B|
         || |N|""".stripMargin,

      """|| |N|
         || |B|
         || |N|
         || |B|""".stripMargin
    )

    sym.map(e => boardToASCII(Board(mn, e))).diff(symStr) shouldBe Set.empty
    sym.size shouldBe symStr.size
    sym.size shouldBe 4
  }

  "symmetrize boards spec 02" in {
    val mn = MxN(4, 3)
    val boardEntries = SortedSet(
      BoardEntry(Position(0, 0), N),
      BoardEntry(Position(3, 0), N))
    val sym = symmetrizeBoard(Board(mn, boardEntries))
    val symStr = Set(
      """||N| | |
         || | | |
         || | | |
         ||N| | |""".stripMargin,

      """|| | |N|
         || | | |
         || | | |
         || | |N|""".stripMargin
    )
    sym.map(e => boardToASCII(Board(mn, e))).diff(symStr) shouldBe Set.empty
    sym.size shouldBe symStr.size
    sym.size shouldBe 2
  }

  "symmetrize boards spec 03" in {
    val mn = MxN(5, 5)
    val boardEntries = SortedSet(
      BoardEntry(Position(2, 2), N))
    val sym = symmetrizeBoard(Board(mn, boardEntries))
    val symStr = Set(
      """|| | | | | |
         || | | | | |
         || | |N| | |
         || | | | | |
         || | | | | |""".stripMargin
    )
    sym.map(e => boardToASCII(Board(mn, e))).diff(symStr) shouldBe Set.empty
    sym.size shouldBe symStr.size
    sym.size shouldBe 1
  }

  "symmetrize boards spec 04" in {
    val mn = MxN(5, 7)
    val boardEntries = SortedSet(
      BoardEntry(Position(2, 3), N))
    val sym = symmetrizeBoard(Board(mn, boardEntries))
    val symStr = Set(
     """|| | | | | | | |
        || | | | | | | |
        || | | |N| | | |
        || | | | | | | |
        || | | | | | | |""".stripMargin
    )
    sym.map(e => boardToASCII(Board(mn, e))).diff(symStr) shouldBe Set.empty
    sym.size shouldBe symStr.size
    sym.size shouldBe 1
  }

  "symmetrize boards spec 05" in {
    val mn = MxN(5, 7)
    val boardEntries = SortedSet(
      BoardEntry(Position(2, 3), Q),
      BoardEntry(Position(2, 5), N))
    val sym = symmetrizeBoard(Board(mn, boardEntries))
    val symStr = Set(
      """|| | | | | | | |
         || | | | | | | |
         || | | |Q| |N| |
         || | | | | | | |
         || | | | | | | |""".stripMargin,

      """|| | | | | | | |
         || | | | | | | |
         || |N| |Q| | | |
         || | | | | | | |
         || | | | | | | |""".stripMargin
    )
    sym.map(e => boardToASCII(Board(mn, e))).diff(symStr) shouldBe Set.empty
    sym.size shouldBe symStr.size
    sym.size shouldBe 2
  }

  "symmetrize boards spec 06" in {
    val mn = MxN(5, 7)
    val boardEntries = SortedSet(
      BoardEntry(Position(2, 3), Q),
      BoardEntry(Position(3, 5), N))
    val sym = symmetrizeBoard(Board(mn, boardEntries))
    val symStr = Set(
      """|| | | | | | | |
         || | | | | | | |
         || | | |Q| | | |
         || | | | | |N| |
         || | | | | | | |""".stripMargin,

      """|| | | | | | | |
         || | | | | |N| |
         || | | |Q| | | |
         || | | | | | | |
         || | | | | | | |""".stripMargin,

      """|| | | | | | | |
         || | | | | | | |
         || | | |Q| | | |
         || |N| | | | | |
         || | | | | | | |""".stripMargin,

      """|| | | | | | | |
         || |N| | | | | |
         || | | |Q| | | |
         || | | | | | | |
         || | | | | | | |""".stripMargin
    )
    sym.map(e => boardToASCII(Board(mn, e))).diff(symStr) shouldBe Set.empty
    sym.size shouldBe symStr.size
    sym.size shouldBe 4
  }

  "boards are diff spec" in {
    val e0 = SortedSet(BoardEntry(Position(0, 0), N), BoardEntry(Position(3, 0), N))
    val e1 = SortedSet(BoardEntry(Position(3, 0), N), BoardEntry(Position(0, 0), N))
    e0 != e1 shouldBe false
  }
}
