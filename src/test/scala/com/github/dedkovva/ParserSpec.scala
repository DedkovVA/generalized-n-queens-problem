package com.github.dedkovva

import com.github.dedkovva.Types._
import org.scalatest.{FreeSpec, Matchers}
import Types.Pieces._
import com.github.dedkovva.Parser.ValidationResult

class ParserSpec extends FreeSpec with Matchers {
  private def validate(args: List[String]): ValidationResult = {
    val parsingResult = Parser.parseRecur(args)
    Parser.validate(args, parsingResult)
  }
  
  "parser spec 01" in {
    val args = ("Q 3   r 2   sst sst    Printboard none   PrintBoard no   sst 2   pp PrintBOARD   " +
      "r q   o 111   r -1  n 5   q q").split("\\s+").toList
    val r = validate(args)

    r.isCorrect shouldBe false
    r.emptyInput shouldBe false
    r.mParamIsNotFound shouldBe true
    r.nParamIsNotFound shouldBe true
    r.repeatingParamsToValues shouldBe Seq(
      "Q" -> Seq("3", "q"),
      "R" -> Seq("2", "q", "-1"),
      "SST" -> Seq("sst", "2"),
      "PRINTBOARD" -> Seq("none", "no"))
    r.intParamsToIncorrectValues shouldBe Seq(
      "Q" -> Seq("q"),
      "R" -> Seq("q", "-1"))
    r.printBoardIncorrectValues shouldBe Seq("no")
    r.unknownParams shouldBe Seq("SST", "PP", "O")
    r.lastParamWoValue shouldBe None
  }

  "parser spec 02" in {
    val args = "-m 3  Q 3   Printboard none   sst sst   PrintBoard no   pp q    o 111   r -1  n 5   q q"
      .split("\\s+").toList
    val r = validate(args)

    r.isCorrect shouldBe false
    r.emptyInput shouldBe false
    r.mParamIsNotFound shouldBe false
    r.nParamIsNotFound shouldBe true
    r.repeatingParamsToValues shouldBe Seq(
      "Q" -> Seq("3", "q"),
      "PRINTBOARD" -> Seq("none", "no"))
    r.intParamsToIncorrectValues shouldBe Seq(
      "Q" -> Seq("q"),
      "R" -> Seq("-1"))
    r.printBoardIncorrectValues shouldBe Seq("no")
    r.unknownParams shouldBe Seq("SST", "PP", "O")
    r.lastParamWoValue shouldBe None
  }

  "parser spec 03" in {
    val args = "-m 3    -n 4    r 2     b 3     k 4     n 5".split("\\s+")
    val r = validate(args.toList)

    r.isCorrect shouldBe true
    r.emptyInput shouldBe false
    r.mParamIsNotFound shouldBe false
    r.nParamIsNotFound shouldBe false
    r.repeatingParamsToValues.isEmpty shouldBe true
    r.intParamsToIncorrectValues.isEmpty shouldBe true
    r.printBoardIncorrectValues.isEmpty shouldBe true
    r.unknownParams.isEmpty shouldBe true
    r.lastParamWoValue shouldBe None

    val inputData = Parser.parseArgs(args).right.get
    inputData.m shouldBe 3
    inputData.n shouldBe 4
    inputData.pieceCountSeq.toSet shouldBe Set(PieceCount(R, 2), PieceCount(B, 3), PieceCount(K, 4), PieceCount(N, 5))
    inputData.printBoard shouldBe PrintBoardValues.None
  }

  "parser spec 04" in {
    val args = "-m 3    -n 4    q 1     r 2     b 3     k 4     q 1     n 5".split("\\s+").toList
    val r = validate(args)

    r.isCorrect shouldBe false
    r.emptyInput shouldBe false
    r.mParamIsNotFound shouldBe false
    r.nParamIsNotFound shouldBe false
    r.repeatingParamsToValues shouldBe Seq(
      "Q" -> Seq("1", "1"))
    r.intParamsToIncorrectValues.isEmpty shouldBe true
    r.printBoardIncorrectValues.isEmpty shouldBe true
    r.unknownParams.isEmpty shouldBe true
    r.lastParamWoValue shouldBe None
  }


  "parser spec 05" in {
    val args = "-m 3    -n 4    q 1     r 2     b 3     k 4     -n 1    n 5".split("\\s+").toList
    val r = validate(args)

    r.isCorrect shouldBe false
    r.emptyInput shouldBe false
    r.mParamIsNotFound shouldBe false
    r.nParamIsNotFound shouldBe false
    r.repeatingParamsToValues shouldBe Seq(
      "-N" -> Seq("4", "1"))
    r.intParamsToIncorrectValues.isEmpty shouldBe true
    r.printBoardIncorrectValues.isEmpty shouldBe true
    r.unknownParams.isEmpty shouldBe true
    r.lastParamWoValue shouldBe None
  }

  "parser spec 06" in {
    val args = "".split("\\s+").toList
    val r = validate(args)

    r.isCorrect shouldBe false
    r.emptyInput shouldBe false
    r.mParamIsNotFound shouldBe true
    r.nParamIsNotFound shouldBe true
    r.repeatingParamsToValues.isEmpty shouldBe true
    r.intParamsToIncorrectValues.isEmpty shouldBe true
    r.printBoardIncorrectValues.isEmpty shouldBe true
    r.unknownParams.isEmpty shouldBe true
    r.lastParamWoValue shouldBe Some("")
  }

  "parser spec 07" in {
    val args =
      s"""
         |
       """.stripMargin.split("\\s+").toList
    val r = validate(args)

    r.isCorrect shouldBe false
    r.emptyInput shouldBe true
    r.mParamIsNotFound shouldBe true
    r.nParamIsNotFound shouldBe true
    r.repeatingParamsToValues.isEmpty shouldBe true
    r.intParamsToIncorrectValues.isEmpty shouldBe true
    r.printBoardIncorrectValues.isEmpty shouldBe true
    r.unknownParams.isEmpty shouldBe true
    r.lastParamWoValue shouldBe None
  }

  "parser spec 08" in {
    val args = "111".stripMargin.split("\\s+").toList
    val r = validate(args)

    r.isCorrect shouldBe false
    r.emptyInput shouldBe false
    r.mParamIsNotFound shouldBe true
    r.nParamIsNotFound shouldBe true
    r.repeatingParamsToValues.isEmpty shouldBe true
    r.intParamsToIncorrectValues.isEmpty shouldBe true
    r.printBoardIncorrectValues.isEmpty shouldBe true
    r.unknownParams.isEmpty shouldBe true
    r.lastParamWoValue shouldBe Some("111")
  }

  "parser spec 09" in {
    val args = "?? ;    . ,,,     .".stripMargin.split("\\s+").toList
    val r = validate(args)

    r.isCorrect shouldBe false
    r.emptyInput shouldBe false
    r.mParamIsNotFound shouldBe true
    r.nParamIsNotFound shouldBe true
    r.repeatingParamsToValues.isEmpty shouldBe true
    r.intParamsToIncorrectValues.isEmpty shouldBe true
    r.printBoardIncorrectValues.isEmpty shouldBe true
    r.unknownParams shouldBe Seq("??", ".")
    r.lastParamWoValue shouldBe Some(".")
  }
}
