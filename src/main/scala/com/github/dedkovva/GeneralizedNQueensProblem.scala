package com.github.dedkovva

import java.util.Date
import scala.annotation.tailrec
import Types._

/*For input [sbt 'run -m 7 -n 7 n 1 k 2 b 2 q 2 PrintBoard none'] result is
  TOTAL COUNT: 3063828
  TOTAL TIME IN SEC: 29.011*/

object GeneralizedNQueensProblem extends App {
  run()

  private def run() = {
    val startTime = new Date()

    val inputDataEither = Parser.parseArgs(args)
    inputDataEither match {
      case Right(inputData) =>
        import inputData.{m, n, printBoard}
        import Engine.{solve, boardToASCII, boardEntriesToPrettyStr}
        import PrintBoardValues.{ASCII, AsLine}

        val taskResult = solve(inputData)

        if (Seq(ASCII, AsLine).map(_.toUpperCase).contains(printBoard.toUpperCase)) {
          val mn = MxN(m, n)
          def print(f: BoardEntries => String) = printBoards(taskResult, mn, f)

          if (printBoard.equalsIgnoreCase(ASCII)) {
            print(boardEntries => {
              val ascii = boardToASCII(Board(mn, boardEntries))
              s"$ascii${System.lineSeparator}"
            })
          } else {
            print(boardEntriesToPrettyStr)
          }
        }

        def totalTimeInSec = ((new Date).getTime - startTime.getTime).toDouble / 1000
        println(s"TOTAL COUNT: ${taskResult.countWithAllReflections}")
        println(s"TOTAL TIME IN SEC: $totalTimeInSec")
      case Left(errorMsg) =>
        println(s"$errorMsg")
    }
  }

  private def printBoards(taskResult: TaskResult, mn: MxN, f: BoardEntries => String): Unit = {
    val printN: Int = 1000
    @tailrec
    def print(i: Int = 0, j: Int = 0): Unit = {
      import taskResult.uniqueBoardEntriesSeq
      if (i < uniqueBoardEntriesSeq.size && j < printN) {
        val boardEntries = uniqueBoardEntriesSeq(i)
        val symBoards = Engine.symmetrizeBoard(Board(mn, boardEntries)).toVector
        val jn = j + symBoards.size
        val min = Math.min(jn, printN)
        for (k <- j until min) {
          println(f(symBoards(k - j)))
        }
        print(i + 1, jn)
      }
    }
    print()
  }
}