package com.github.dedkovva

import scala.annotation.tailrec
import Types._
import Types.Pieces._

import scala.collection.immutable.SortedSet

private object Engine {
  private[dedkovva] implicit object BoardEntryOrdering extends Ordering[BoardEntry] {
    override def compare(x: BoardEntry, y: BoardEntry): Int = {
      Ordering[(Int, Int, Char)].compare(
        (x.position.i, x.position.j, x.piece.name),
        (y.position.i, y.position.j, y.piece.name))
    }
  }

  private[dedkovva] def solve(inputData: InputData): TaskResult = {
    import inputData.{m, n, pieceCountSeq}
    val mn = MxN(m, n)
    val pieces = sortPieceCountSeq(pieceCountSeq).flatMap(e => Seq.fill[Piece](e.count)(e.piece))
    solve(TaskParams(mn, pieces))
  }

  private def solve(taskParams: TaskParams): TaskResult = {
    import taskParams.{mn, pieces}
    import mn.{m, n}
    if (pieces.isEmpty || m <= 0 || n <= 0 || m * n < pieces.length) {
      TaskResult(Vector.empty, 0)
    } else {
      val positions = (for (i <- 0 until m; j <- 0 until n) yield Position(i, j)).toVector
      val boardEntriesSeq = positions.map(_ => SortedSet.empty[BoardEntry])
      val availablePositionsSeq = positions.map(_ => positions)
      val pieceIteration = PieceIteration(0, boardEntriesSeq, availablePositionsSeq)
      solveRecur(TaskParams(mn, pieces), pieceIteration)
    }
  }

  @tailrec
  private def solveRecur(taskParams: TaskParams,
                         pieceIteration: PieceIteration
                        ): TaskResult = {
    val accumulationResult = accumulateForPieces(PiecesIterConst(taskParams, pieceIteration))

    import accumulationResult.{boardEntriesSeq, countWithAllReflections, availablePositionsSeq}
    import pieceIteration.pieceIndex

    if (pieceIndex == taskParams.pieces.length - 1) {
      TaskResult(boardEntriesSeq, countWithAllReflections)
    } else {
      val pieceIteration = PieceIteration(pieceIndex + 1, boardEntriesSeq, availablePositionsSeq)
      solveRecur(taskParams, pieceIteration)
    }
  }

  @tailrec
  private def accumulateForPieces(iterConst: PiecesIterConst,
                                  iterIndices: PiecesIterIndices = PiecesIterIndices(),
                                  prevResult: AccumulationResult = AccumulationResult()
                                 ): AccumulationResult = {
    import iterConst.{taskParams, pieceIteration}
    import taskParams.{mn, pieces}
    import pieceIteration.{pieceIndex, boardEntriesSeq, availablePositionsSeq}
    import iterIndices.{boardEntriesIndex, posIndex}

    if (boardEntriesIndex == boardEntriesSeq.size) {
      prevResult
    } else {
      val availablePositions = availablePositionsSeq(boardEntriesIndex)
      val nextIterIndices = evalNextIterIndices(iterIndices, availablePositions)

      if (availablePositions.size > posIndex) {
        val pos = availablePositions(posIndex)
        val piece = pieces(pieceIndex)
        val boardEntry = BoardEntry(pos, piece)
        val positionsCandidate = availablePositions.filter(ap => isSafePosition(boardEntry, ap))
        val unprocessedPiecesCount = pieces.length - pieceIndex - 1
        val boardEntries = boardEntriesSeq(boardEntriesIndex)

        if (positionsCandidate.size >= unprocessedPiecesCount && isSafeOfEntry(boardEntries, boardEntry)) {
          val boardEntriesCandidate = boardEntries + boardEntry
          val boardCandidate = Board(mn, boardEntriesCandidate)
          val symCountOpt = noOneContainsSymWithCount(prevResult.boardEntriesStr, boardCandidate)

          if (symCountOpt.isDefined) {
            val symCount = symCountOpt.get
            val accumulatorValues = AccumulatorValues(boardEntriesCandidate, positionsCandidate, symCount)
            val nextResult = accumulate(pieceIndex, pieces.length, accumulatorValues, prevResult)
            accumulateForPieces(iterConst, nextIterIndices, nextResult)
          } else {
            accumulateForPieces(iterConst, nextIterIndices, prevResult)
          }
        } else {
          accumulateForPieces(iterConst, nextIterIndices, prevResult)
        }
      } else {
        accumulateForPieces(iterConst, nextIterIndices, prevResult)
      }
    }
  }

  private def evalNextIterIndices(iterIndices: PiecesIterIndices,
                                  availablePositions: Vector[Position]): PiecesIterIndices = {
    import iterIndices._
    val (nextBoardIndex, nextPosIndex) = {
      if (posIndex == availablePositions.size - 1 || availablePositions.isEmpty) (boardEntriesIndex + 1, 0)
      else (boardEntriesIndex, posIndex + 1)
    }
    PiecesIterIndices(nextBoardIndex, nextPosIndex)
  }

  private def accumulate(pieceIndex: Int,
                         piecesLen: Int,
                         accumulatorValues: AccumulatorValues,
                         prevResult: AccumulationResult): AccumulationResult = {
    import accumulatorValues.{boardEntries, availablePositions, symCount}
    import prevResult.{boardEntriesSeq, boardEntriesStr, availablePositionsSeq, countWithAllReflections}

    def evalAccumulationResult(availablePositionsSeq: Vector[Vector[Position]],
                               countWithAllReflections: Int): AccumulationResult = {
      val newBoardEntriesSeq: Vector[BoardEntries] = boardEntries +: boardEntriesSeq
      val newBoardEntriesStr: SortedSet[String] = boardEntriesStr + boardEntriesToStr(boardEntries)
      AccumulationResult(newBoardEntriesSeq, newBoardEntriesStr, availablePositionsSeq, countWithAllReflections)
    }

    if (pieceIndex == piecesLen - 1) {
      val newCountWithAllReflections: Int = countWithAllReflections + symCount
      evalAccumulationResult(availablePositionsSeq, newCountWithAllReflections)
    } else {
      val newAvailablePositionsSeq: Vector[Vector[Position]] = availablePositions +: availablePositionsSeq
      evalAccumulationResult(newAvailablePositionsSeq, countWithAllReflections)
    }
  }

  private def isSafeOfEntry(boardEntries: BoardEntries, boardEntry: BoardEntry): Boolean = {
    boardEntries.forall(e => isSafePosition(boardEntry, e.position))
  }

  private[dedkovva] def isSafePosition(boardEntry: BoardEntry, position: Position): Boolean = {
    val piece = boardEntry.piece
    val entryPosition = boardEntry.position
    piece match {
      case Q =>
        isSafeStraight(entryPosition, position) && isSafeDiagonal(entryPosition, position)
      case R =>
        isSafeStraight(entryPosition, position)
      case B =>
        isSafeDiagonal(entryPosition, position)
      case K =>
        isSafeKing(entryPosition, position)
      case N =>
        isSafeKnight(entryPosition, position)
    }
  }

  private[dedkovva] def isSafeStraight(p1: Position, p2: Position): Boolean = {
    p1.i != p2.i && p1.j != p2.j
  }

  private[dedkovva] def isSafeDiagonal(p1: Position, p2: Position): Boolean = {
    Math.abs(p2.i - p1.i) != Math.abs(p2.j - p1.j)
  }

  private[dedkovva] def isSafeKing(p1: Position, p2: Position): Boolean = {
    Math.abs(p2.i - p1.i) > 1 || Math.abs(p2.j - p1.j) > 1
  }

  private[dedkovva] def isSafeKnight(p1: Position, p2: Position): Boolean = {
    !((p1.i == p2.i && p1.j == p2.j) ||
      (Math.abs(p2.i - p1.i) == 2 && Math.abs(p2.j - p1.j) == 1) ||
      (Math.abs(p2.i - p1.i) == 1 && Math.abs(p2.j - p1.j) == 2))
  }

  private def noOneContainsSymWithCount(boardEntriesStr: scala.collection.Set[String],
                                        board: Board): Option[Int] = {
    val sym = symmetrizeBoardStr(board)
    val noOneContains = sym.forall(e => !boardEntriesStr.contains(e))
    if (noOneContains) Option(sym.size) else None
  }

  private def symmetrizeBoardStr(board: Board): Set[String] = {
    val symBoardEntries = symmetrizeBoard(board)
    symBoardEntries.map(boardEntriesToStr)
  }

  private[dedkovva] def symmetrizeBoard(board: Board): Set[BoardEntries] = {
    import board.mn.{m, n}

    if (m == n) {
      rotateSquareBoard(board)
    } else {
      reflectRectangularBoard(board)
    }
  }

  private def rotateSquareBoard(board: Board): Set[BoardEntries] = {
    import board.mn
    import board.mn.{m, n}

    require(m == n)

    import scala.language.implicitConversions
    implicit def boardEntriesToBoard(boardEntries: BoardEntries): Board = Board(mn, boardEntries)

    val b2 = rotateSquareBoardCounterWise(board)
    val b3 = rotateSquareBoardCounterWise(b2)
    val b4 = rotateSquareBoardCounterWise(b3)

    val bH1 = reflectHorizontal(board)
    val bH2 = rotateSquareBoardCounterWise(bH1)
    val bH3 = rotateSquareBoardCounterWise(bH2)
    val bH4 = rotateSquareBoardCounterWise(bH3)

    Set(board.boardEntries, b2, b3, b4, bH1, bH2, bH3, bH4)
  }

  private def reflectRectangularBoard(board: Board): Set[BoardEntries] = {
    val bH = reflectHorizontal(board)
    val bV = reflectVertical(board)
    val bHV = reflectVertical(Board(board.mn, bH))

    Set(board.boardEntries, bH, bV, bHV)
  }

  private[dedkovva] def rotateSquareBoardCounterWise(board: Board): BoardEntries = {
    import board.{mn, boardEntries}
    import mn.{m, n}

    require(m == n)

    boardEntries.map(e => {
      val i = n - e.position.j - 1
      val j = e.position.i
      BoardEntry(Position(i, j), e.piece)
    })
  }

  private def reflectHorizontal(board: Board): BoardEntries = {
    import board.{mn, boardEntries}

    boardEntries.map(e => {
      val i = mn.m - e.position.i - 1
      val j = e.position.j
      BoardEntry(Position(i, j), e.piece)
    })
  }

  private def reflectVertical(board: Board): BoardEntries = {
    import board.{mn, boardEntries}

    boardEntries.map(e => {
      val i = e.position.i
      val j = mn.n - e.position.j - 1
      BoardEntry(Position(i, j), e.piece)
    })
  }

  private def boardEntryToStr(e: BoardEntry) = s"${e.position.i}:${e.position.j}:${e.piece}"
  private def boardEntriesToStr(boardEntries: BoardEntries, delimiter: String) =
    boardEntries.map(boardEntryToStr).mkString(delimiter)

  private def boardEntriesToStr(boardEntries: BoardEntries): String = {
    boardEntriesToStr(boardEntries, "")
  }

  private[dedkovva] def boardEntriesToPrettyStr(boardEntries: BoardEntries): String = {
    boardEntriesToStr(boardEntries, "-")
  }

  private[dedkovva] def boardToASCII(board: Board): String = {
    import board.{mn, boardEntries}
    import mn.{m, n}

    val positionToPieceMap = boardEntries.map(e => e.position -> e.piece).toMap
    val s = new StringBuilder
    for (i <- 0 until m; j <- 0 until n) {
      if (j == 0) {
        s.append("|")
      }
      val position = Position(i, j)
      if (positionToPieceMap.contains(position)) {
        s.append(positionToPieceMap(position))
      } else {
        s.append(" ")
      }
      s.append("|")
      if (j == (n - 1) && i != (m - 1)) {
        s.append(System.lineSeparator())
      }
    }
    s.toString()
  }

  //We need this bcz best solution with 'strong' pieces at the beginning
  private[dedkovva] def sortPieceCountSeq(pieceCountSeq: Seq[PieceCount]): Seq[PieceCount] = {
    pieceCountSeq.sortWith((pc1, pc2) => {
      pc1.piece.locationPriority > pc2.piece.locationPriority
    })
  }
}
