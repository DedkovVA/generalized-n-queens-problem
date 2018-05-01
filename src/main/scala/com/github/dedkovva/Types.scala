package com.github.dedkovva

import scala.collection.immutable.SortedSet

private[dedkovva] object Types {
  //Best time with mutable collections
  private[dedkovva] type BoardEntries = SortedSet[BoardEntry]

  private[dedkovva] object Pieces {
    private[dedkovva] sealed trait Piece {
      val name: Char
      val locationPriority: Int
    }
    private[dedkovva] object Piece {
      val values = Set(K, Q, B, R, N)
    }
    private[dedkovva] case object Q extends Piece { val name = 'Q'; val locationPriority = 4 }//Queen
    private[dedkovva] case object R extends Piece { val name = 'R'; val locationPriority = 3 }//Rook
    private[dedkovva] case object B extends Piece { val name = 'B'; val locationPriority = 2 }//Bishop
    private[dedkovva] case object K extends Piece { val name = 'K'; val locationPriority = 1 }//King
    private[dedkovva] case object N extends Piece { val name = 'N'; val locationPriority = 0 }//Knight
  }

  import Pieces.Piece

  private[dedkovva] case class Position(i: Int, j: Int)
  private[dedkovva] case class MxN(m: Int, n: Int)
  private[dedkovva] case class Board(mn: MxN, boardEntries: BoardEntries)
  private[dedkovva] case class BoardEntry(position: Position, piece: Piece)

  private[dedkovva] case class PieceCount(piece: Piece, count: Int)
  private[dedkovva] case class InputData(m: Int, n: Int, pieceCountSeq: Seq[PieceCount], printBoard: String)

  private[dedkovva] case class TaskParams(mn: MxN, pieces: Seq[Piece])
  private[dedkovva] case class TaskResult(uniqueBoardEntriesSeq: Vector[BoardEntries], countWithAllReflections: Int)

  private[dedkovva] case class AccumulationResult(
                                                   boardEntriesSeq: Vector[BoardEntries] = Vector.empty,
                                                   boardEntriesStr: SortedSet[String] = SortedSet.empty,
                                                   availablePositionsSeq: Vector[Vector[Position]] = Vector.empty,
                                                   countWithAllReflections: Int = 0
                                                 )
  private[dedkovva] case class AccumulatorValues(
                                                  boardEntries: BoardEntries,
                                                  availablePositions: Vector[Position],
                                                  symCount: Int
                                                )

  private[dedkovva] case class PiecesIterConst(taskParams: TaskParams, pieceIteration: PieceIteration)
  private[dedkovva] case class PiecesIterIndices(boardEntriesIndex: Int = 0, posIndex: Int = 0)
  private[dedkovva] case class PieceIteration(
                                               pieceIndex: Int,
                                               boardEntriesSeq: Vector[BoardEntries],
                                               availablePositionsSeq: Vector[Vector[Position]]
                                             )

  private[dedkovva] case class ParsingResult(
                                              raw: Map[String, Seq[String]] = Map.empty,
                                              lastParamWoValue: Option[String] = None
                                            )

  private[dedkovva] object CmdLineTaskParams {
    val PrintBoard = "PrintBoard"
    val PieceValues = Piece.values.map(_.name.toString)
    val M = "-M"
    val N = "-N"
    val MN = Seq(M, N)
    val IntParams = PieceValues ++ MN
    val AllUpperCasedParams = IntParams ++ Seq(PrintBoard.toUpperCase)
  }

  private[dedkovva] object PrintBoardValues {
    val None = "none"
    val ASCII = "ascii"
    val AsLine = "AsLine"
    val values = Seq(None, ASCII, AsLine)
  }
}
