package com.github.dedkovva

import org.scalatest.{FreeSpec, Matchers}

import Types._
import Types.Pieces._

class PrintBoardsSpec extends FreeSpec with Matchers {
  import Engine._

  def evalAllPossibleBoards(inputData: InputData, f: BoardEntries => String): Seq[String] = {
    import inputData.{m, n}
    val boardEntriesSeq = solve(inputData).uniqueBoardEntriesSeq
    boardEntriesSeq.flatMap(e => symmetrizeBoard(Board(MxN(m, n), e))).map(e => f(e))
  }

  val mn = MxN(2, 2)
  val inputData = InputData(mn.m, mn.n, Seq(PieceCount(N, 1), PieceCount(R, 1)), PrintBoardValues.None)

  "diff spec" in {
    Seq(1, 2, 3).diff(Seq(3, 2, 1)) shouldBe Seq.empty
    Seq(1, 1, 2, 3).diff(Seq(3, 2, 1)) shouldBe Seq(1)
    Seq(1, 1, 2, 3).diff(Seq(3, 2, 1, 1)) shouldBe Seq.empty
    Seq(1, 1, 2, 3).diff(Seq(3, 2, 1, 1, 1)) shouldBe Seq.empty
    Seq(1, 1, 2, 3).diff(Seq(4, 3, 2, 1, 1, 1)) shouldBe Seq.empty
    Seq(1, 1, 2, 3).diff(Seq(1, 3, 2, 1, 1)) shouldBe Seq.empty
  }

  "ASCII print spec" in {
    val r = evalAllPossibleBoards(inputData, (e: BoardEntries) => boardToASCII(Board(mn, e)))
    val expected = Seq(
      """||R| |
         || |N|""".stripMargin,

      """|| |N|
         ||R| |""".stripMargin,

      """||N| |
         || |R|""".stripMargin,

      """|| |R|
         ||N| |""".stripMargin)
    r.size shouldBe expected.size
    r.size shouldBe 4
    r.diff(expected) shouldBe Seq.empty
  }

  "AsLine print spec" in {
    val r = evalAllPossibleBoards(inputData, boardEntriesToPrettyStr)
    val expected = Seq(
      "0:0:R-1:1:N",
      "0:1:N-1:0:R",
      "0:0:N-1:1:R",
      "0:1:R-1:0:N")
    r.size shouldBe expected.size
    r.size shouldBe 4
    r.diff(expected) shouldBe Seq.empty
  }
}
