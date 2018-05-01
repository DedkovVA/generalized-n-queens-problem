package com.github.dedkovva

import org.scalatest.{FreeSpec, Matchers}
import Types._

class AppSpec extends FreeSpec with Matchers {
  import Engine._

  import com.github.dedkovva.Parser._

  import scala.language.implicitConversions
  implicit def splitStrToArray(s: String) = s.split("\\s+")
  implicit def eitherToBoard(e: Either[String, InputData]) = e.right.get

  def evalAllPossibleBoards(inputData: InputData): Int = {
    solve(inputData).countWithAllReflections
  }

  "1х1, 1 Rook" in {
    evalAllPossibleBoards(parseArgs("-m 1 -n 1 R 1 q 0 b 0 k 0 n 0")) shouldBe 1
  }

  "2х1, 2 Bishops" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 1 b 2 q 0 r 0 k 0 n 0")) shouldBe 1
  }

  "2х1, 2 Rooks" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 1 r 2 q 0 b 0 k 0 n 0")) shouldBe 0
  }

  "2х1, 2 Knights" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 1 n 2 q 0 r 0 b 0 k 0")) shouldBe 1
  }

  "2х2, 1 Knight" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 2 n 1 q 0 b 0 r 0 k 0")) shouldBe 4
  }

  "2х2, 2 Knights" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 2 n 2 q 0 r 0 b 0 k 0")) shouldBe 6
  }

  "2х2, 2 Bishops" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 2 b 2 q 0 r 0 k 0 n 0")) shouldBe 4
  }

  "2х2, 3 Knights" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 2 q 0 r 0 b 0 k 0 n 3")) shouldBe 4
  }

  "2х2, 4 Knights" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 2 q 0 r 0 b 0 k 0 n 4")) shouldBe 1
  }

  "3х3, 2 Queens" in {
    evalAllPossibleBoards(parseArgs("-m 3 -n 3 b 0 q 2 r 0 k 0 n 0")) shouldBe 8
  }

  "3х3, 1 Queen, 1 Knight" in {
    evalAllPossibleBoards(parseArgs("-m 3 -n 3 b 0 q 1 r 0 k 0 n 1")) shouldBe 0
  }

  "3х3, 3 Knights" in {
    evalAllPossibleBoards(parseArgs("-m 3 -n 3 q 0 r 0 b 0 k 0 n 3")) shouldBe 36
  }

  "3х3, 3 Kings" in {
    evalAllPossibleBoards(parseArgs("-m 3 -n 3 q 0 r 0 b 0 k 3 n 0")) shouldBe 8
  }

  "3х3, 1 King, 1 Knight" in {
    evalAllPossibleBoards(parseArgs("-m 3 -n 3 q 0 r 0 b 0 k 1 n 1")) shouldBe 16
  }

  "3х3, 2 Kings, 1 Knight" in {
    evalAllPossibleBoards(parseArgs("-m 3 -n 3 q 0 r 0 b 0 k 2 n 1")) shouldBe 12
  }

  "3х3, 2 Kings, 2 Knights" in {
    evalAllPossibleBoards(parseArgs("-m 3 -n 3 q 0 r 0 b 0 k 2 n 2")) shouldBe 6
  }

  "3х3, 2 Kings, 3 Knights" in {
    evalAllPossibleBoards(parseArgs("-m 3 -n 3 q 0 r 0 b 0 k 2 n 3")) shouldBe 0
  }

  "3х3, 2 Kings, 2 Knights, 1 Rook" in {
    evalAllPossibleBoards(parseArgs("-m 3 -n 3 q 0 r 1 b 0 k 2 n 2")) shouldBe 0
  }

  "3х3, 2 Kings, 2 Knights, 1 Bishop" in {
    evalAllPossibleBoards(parseArgs("-m 3 -n 3 q 0 r 0 b 1 k 2 n 2")) shouldBe 4
  }

  "3х4, 1 Queen, 1 King, 1 Knight" in {
    evalAllPossibleBoards(parseArgs("-m 3 -n 4 q 1 r 0 b 0 k 1 n 1")) shouldBe 8
  }

  "2х4, 2 Bishops, 1 Knight" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 4 q 0 r 0 b 2 k 0 n 1")) shouldBe 48
  }

  "2х4, 2 Bishops, 1 Knight -- 1" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 4 q 0 r 0 printboard none b 2 k 0 n 1")) shouldBe 48
  }

  "2х4, 2 Bishops, 1 Knight -- 2" in {
    evalAllPossibleBoards(parseArgs("printboard ASCII -m 2 -n 4 q 0 r 0 b 2 k 0 n 1")) shouldBe 48
  }

  "2х4, 2 Bishops, 1 Knight -- 3" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 4 q 0 r 0 b 2 k 0 n 1 printboard ASLINE")) shouldBe 48
  }

  "2х4, 2 Bishops, 1 Knight -- 4" in {
    evalAllPossibleBoards(parseArgs("-m 2 -N 4 q 0 r 0 b 2 k 0 n 1 printboard  ASLINE")) shouldBe 48
  }

  "2х4, 2 Bishops, 1 Knight, 1 King" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 4 q 0 r 0 b 2 k 1 n 1")) shouldBe 0
  }

  "2х4, 2 Bishops, 2 Knights" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 4 q 0 r 0 b 2 k 0 n 2")) shouldBe 30
  }

  "2х4, 2 Rooks" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 4 q 0 r 2 b 0 k 0 n 0")) shouldBe 12
  }

  "2х4, 2 Rooks, 1 Bishop" in {
    evalAllPossibleBoards(parseArgs("-m 2 -n 4 q 0 r 2 b 1 k 0 n 0")) shouldBe 0
  }

  "3х3, 2 Kings, 1 Rook" in {
    evalAllPossibleBoards(parseArgs("-m 3 -n 3 q 0 r 1 b 0 k 2 n 0")) shouldBe 4
  }

  "4х4, 2 Rooks, 4 Knights" in {
    evalAllPossibleBoards(parseArgs("-m 4 -n 4 q 0 r 2 b 0 k 0 n 4")) shouldBe 8
  }

  "3х3, 1 Queen, 1 King" in {
    evalAllPossibleBoards(parseArgs("-m 3 -n 3 q 1 r 0 b 0 k 1 n 0")) shouldBe 16
  }

  "7х7, 7 Queens" in {
    evalAllPossibleBoards(parseArgs("-m 7 -n 7 q 7 r 0 b 0 k 0 n 0")) shouldBe 40
  }

  "8х8, 8 Queens" in {
    evalAllPossibleBoards(parseArgs("-m 8 -n 8 q 8 r 0 b 0 k 0 n 0")) shouldBe 92
  }

  "0х0, 8 Queens" in {
    evalAllPossibleBoards(parseArgs("-m 0 -n 0 q 8 r 0 b 0 k 0 n 0")) shouldBe 0
  }

  "8х8, 0 pieces" in {
    evalAllPossibleBoards(parseArgs("-m 8 -n 8 q 0 r 0 b 0 k 0 n 0")) shouldBe 0
  }

  "0х0, 0 pieces" in {
    evalAllPossibleBoards(parseArgs("-m 0 -n 0 q 0 r 0 b 0 k 0 n 0")) shouldBe 0
  }

  "8х8, 1 Queen" in {
    evalAllPossibleBoards(parseArgs("-m 8 -n 8 q 1 r 0 b 0 k 0 n 0")) shouldBe 64
  }

  "1х0, 8 Queens" in {
    evalAllPossibleBoards(parseArgs("-m 1 -n 0 q 8 r 0 b 0 k 0 n 0")) shouldBe 0
  }

  "1х0, 1 Queen" in {
    evalAllPossibleBoards(parseArgs("-m 1 -n 0 q 1 r 0 b 0 k 0 n 0")) shouldBe 0
  }

  "1х1, 1 Queen" in {
    evalAllPossibleBoards(parseArgs("-m 1 -n 1 q 1 r 0 b 0 k 0 n 0")) shouldBe 1
  }

  "1х2, 1 Queen" in {
    evalAllPossibleBoards(parseArgs("-m 1 -n 2 q 1 r 0 b 0 k 0 n 0")) shouldBe 2
  }

  "1х1, 2 Queens" in {
    evalAllPossibleBoards(parseArgs("-m 1 -n 1 q 2 r 0 b 0 k 0 n 0")) shouldBe 0
  }

  "1х1, -1 Queen" in {
    parseArgs("-m 1 -n 1 q -1 r 0 b 0 k 0 n 0").isLeft shouldBe true
  }

  "7х7, 2 Kings, 2 Queens, 2 Bishops, 1 Knight" in {
    evalAllPossibleBoards(parseArgs("-m 7 -n 7 n 1 k 2 b 2 r 0 q 2")) shouldBe 3063828
  }
}
