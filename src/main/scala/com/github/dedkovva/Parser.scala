package com.github.dedkovva

import scala.util.Try
import Types._
import com.github.dedkovva.Types.Pieces.Piece

import scala.annotation.tailrec

private object Parser {
  private[dedkovva] def parseArgs(args: Array[String]): Either[String, InputData] = {
    try {
      val parsingResult = parseRecur(args.toList)
      val validationResult = validate(args.toList, parsingResult)
      if (validationResult.isCorrect) {
        val inputData = extractInputData(parsingResult, validationResult)
        Right(inputData)
      } else {
        Left(validationResult.toMsg.get)
      }
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        Left(s"Error occurred while parsing input args: [${e.getMessage}]")
    }
  }

  private def extractInputData(parsingResult: ParsingResult, validationResult: ValidationResult): InputData = {
    import parsingResult._
    import CmdLineTaskParams._

    val paramToInt = raw.filter(e => IntParams.contains(e._1)).map(e => (e._1, e._2.head.toInt))
    val pieceCountSeq = Piece.values.map(e => PieceCount(e, paramToInt.find(_._1 == e.name.toString)
      .map(_._2).getOrElse(0))).filter(_.count != 0).toSeq
    val printBoard = raw.find(_._1 == PrintBoard.toUpperCase).map(_._2.head).getOrElse(PrintBoardValues.None)
    def getBoardSize(param: String) = paramToInt.find(_._1 == param).get._2
    InputData(getBoardSize(M), getBoardSize(N), pieceCountSeq, printBoard)
  }

  @tailrec
  private[dedkovva] def parseRecur(args: List[String],
                                   parsingResult: ParsingResult = ParsingResult()): ParsingResult = {
    args match {
      case k :: v :: tail =>
        parseRecur(tail, ParsingResult(put(parsingResult.raw, k.toUpperCase() -> v), parsingResult.lastParamWoValue))
      case last :: Nil =>
        parseRecur(Nil, ParsingResult(parsingResult.raw, Option(last)))
      case Nil => parsingResult
    }
  }

  private def put(map: Map[String, Seq[String]], e: (String, String)): Map[String, Seq[String]] =
    if (map.contains(e._1)) map + (e._1 -> (e._2 +: map(e._1)))
    else  map + (e._1 -> Seq(e._2))

  private[dedkovva] def validate(args: List[String], parsingResult: ParsingResult): ValidationResult = {
    import CmdLineTaskParams._

    val rawOrderedInValues = parsingResult.raw.map(e => (e._1, e._2.reverse))
    val keyArgs = args.zipWithIndex.filter(_._2 % 2 == 0).map(_._1.toUpperCase())
    val ordered = rawOrderedInValues.toList.sortWith((e1, e2) => keyArgs.indexOf(e1._1) < keyArgs.indexOf(e2._1))

    val emptyInput = args.isEmpty
    def boardSizeIsNotFound(param: String) = !ordered.map(_._1).contains(param)
    val mParamIsNotFount = boardSizeIsNotFound(M)
    val nParamIsNotFount = boardSizeIsNotFound(N)
    val repeatingParamsToValues = ordered.filter(_._2.size > 1)
    val intParamsToIncorrectValues = ordered.filter(e => IntParams.contains(e._1)).map(e => (e._1, e._2.filter(s => {
      val t = Try { s.toInt }
      t.isFailure || t.get < 0
    }))).filter(_._2.nonEmpty)
    val printBoardIncorrectValues = ordered.find(_._1 == PrintBoard.toUpperCase)
      .map(_._2.filter(e => !PrintBoardValues.values.map(_.toUpperCase).contains(e.toUpperCase))).getOrElse(Seq.empty)
    val unknownParams = ordered.map(_._1).filter(e => !AllUpperCasedParams.contains(e))
    val lastParamWoValue = parsingResult.lastParamWoValue
    ValidationResult(emptyInput, mParamIsNotFount, nParamIsNotFount,
      repeatingParamsToValues, intParamsToIncorrectValues, printBoardIncorrectValues, unknownParams, lastParamWoValue)
  }

  private[dedkovva] case class ValidationResult(emptyInput: Boolean = false,
                                                mParamIsNotFound: Boolean = false,
                                                nParamIsNotFound: Boolean = false,
                                                repeatingParamsToValues: Seq[(String, Seq[String])] = Seq.empty,
                                                intParamsToIncorrectValues: Seq[(String, Seq[String])] = Seq.empty,
                                                printBoardIncorrectValues: Seq[String] = Seq.empty,
                                                unknownParams: Seq[String] = Seq.empty,
                                                lastParamWoValue: Option[String] = None) {
    private[dedkovva] def isCorrect: Boolean =
      !emptyInput && !mParamIsNotFound && !nParamIsNotFound && repeatingParamsToValues.isEmpty &&
        intParamsToIncorrectValues.isEmpty && printBoardIncorrectValues.isEmpty && unknownParams.isEmpty &&
        lastParamWoValue.isEmpty

    private[dedkovva] def toMsg: Option[String] = {
      if (isCorrect) None else Option({
        val ls = System.lineSeparator
        def mnMsg(s: String) = s"* Input string should contain $s param with value$ls"

        s"Wrong input format:$ls" +
          when(emptyInput) { s"* Input string must not be empty$ls" } +
          when(mParamIsNotFound) { mnMsg("-m") } +
          when(nParamIsNotFound) { mnMsg("-n") } +
          when(repeatingParamsToValues.nonEmpty) {
            repeatingParamsToValues.map(e =>
              s"* Param [${canonicalName(e._1)}] appears [${e._2.size}] times with values: " +
                s"[${e._2.mkString(", ")}]").mkString(s"$ls") + ls
          } +
          when(intParamsToIncorrectValues.nonEmpty) {
            intParamsToIncorrectValues.map(e =>
              s"* Param [${canonicalName(e._1)}] has non-integer and/or negative value(s): " +
                s"[${e._2.mkString(", ")}]").mkString(s"$ls") + ls
          } +
          when(unknownParams.nonEmpty) {
            s"* There is one or more unknown params: [${unknownParams.map(canonicalName).mkString(", ")}]$ls"
          } +
          when(printBoardIncorrectValues.nonEmpty) {
            s"* There is one or more PrintBoard param with incorrect value(s): " +
              s"[${printBoardIncorrectValues.mkString(", ")}]$ls"
          } +
          when(lastParamWoValue.nonEmpty) { s"* Last param [${lastParamWoValue.get}] without value$ls" } +
          "For correct input format see file readme.md"
      })
    }

    private def canonicalName(s: String): String = {
      import CmdLineTaskParams.PrintBoard
      if (s.equalsIgnoreCase(PrintBoard)) PrintBoard else s.toLowerCase
    }

    private def when(condition: Boolean)(onTrue: => String): String = if (condition) onTrue else ""
  }
}