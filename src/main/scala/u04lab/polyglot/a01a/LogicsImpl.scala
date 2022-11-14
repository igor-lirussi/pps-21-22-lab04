package u04lab.polyglot.a01a
import Logics.*
import u04lab.polyglot.Pair
import u04lab.code.List
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val boat: Int) extends Logics:
  private case class BoatCell(hit:Boolean, coordinates:(Int, Int))
  private var hits = size*size
  import u04lab.code.List.*
  private var boatList :List[BoatCell]= Nil()
  for _ <- 1 to boat do
    boatList = List.append(boatList, Cons(BoatCell(false, (Random.nextInt(size),Random.nextInt(size))), Nil()))
  println(boatList)
  def hit(row: Int, col: Int): Result =
    val shot = BoatCell(false, (row, col))
    hits = hits - 1
    boatList match
      case List.Cons(head, tail) if head==shot =>
        boatList = List.remove(boatList)(cell => cell == shot)
        if List.length(boatList) == 0 then return Result.WON
        Result.HIT
      case _ if hits==0 => Result.LOST
      case _ => Result.MISS

