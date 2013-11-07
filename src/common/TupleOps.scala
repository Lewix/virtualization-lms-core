package scala.virtualization.lms
package common

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter

/**
 * Taken char-for-char from the delite-develop branch of lms
 */

trait TupleOps extends Base {
  val tuple_elems: Seq[String]

  implicit def make_tuple2[A:Manifest,B:Manifest](t: (Rep[A], Rep[B]))(implicit pos: SourceContext) : Rep[(A,B)]
  implicit def make_tuple3[A:Manifest,B:Manifest,C:Manifest](t: (Rep[A], Rep[B], Rep[C]))(implicit pos: SourceContext) : Rep[(A,B,C)]
  implicit def make_tuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](t: (Rep[A], Rep[B], Rep[C], Rep[D]))(implicit pos: SourceContext) : Rep[(A,B,C,D)]
  implicit def make_tuple5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](t: (Rep[A], Rep[B], Rep[C], Rep[D], Rep[E]))(implicit pos: SourceContext) : Rep[(A,B,C,D,E)]

  implicit def t2[A:Manifest,B:Manifest](t: Rep[(A,B)])(implicit pos: SourceContext) =
    ((tuple2_get1(t),tuple2_get2(t)))
  implicit def t3[A:Manifest,B:Manifest,C:Manifest](t: Rep[(A,B,C)])(implicit pos: SourceContext) =
    ((tuple3_get1(t),tuple3_get2(t),tuple3_get3(t)))
  implicit def t4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](t: Rep[(A,B,C,D)])(implicit pos: SourceContext) =
    ((tuple4_get1(t),tuple4_get2(t),tuple4_get3(t),tuple4_get4(t)))
  implicit def t5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](t: Rep[(A,B,C,D,E)])(implicit pos: SourceContext) =
    ((tuple5_get1(t),tuple5_get2(t),tuple5_get3(t),tuple5_get4(t),tuple5_get5(t)))

  def tuple2_get1[A:Manifest](t: Rep[(A,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple2_get2[B:Manifest](t: Rep[(_,B)])(implicit pos: SourceContext) : Rep[B]

  def tuple3_get1[A:Manifest](t: Rep[(A,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple3_get2[B:Manifest](t: Rep[(_,B,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple3_get3[C:Manifest](t: Rep[(_,_,C)])(implicit pos: SourceContext) : Rep[C]

  def tuple4_get1[A:Manifest](t: Rep[(A,_,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple4_get2[B:Manifest](t: Rep[(_,B,_,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple4_get3[C:Manifest](t: Rep[(_,_,C,_)])(implicit pos: SourceContext) : Rep[C]
  def tuple4_get4[D:Manifest](t: Rep[(_,_,_,D)])(implicit pos: SourceContext) : Rep[D]

  def tuple5_get1[A:Manifest](t: Rep[(A,_,_,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple5_get2[B:Manifest](t: Rep[(_,B,_,_,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple5_get3[C:Manifest](t: Rep[(_,_,C,_,_)])(implicit pos: SourceContext) : Rep[C]
  def tuple5_get4[D:Manifest](t: Rep[(_,_,_,D,_)])(implicit pos: SourceContext) : Rep[D]
  def tuple5_get5[E:Manifest](t: Rep[(_,_,_,_,E)])(implicit pos: SourceContext) : Rep[E]
}

trait TupleOpsExp extends TupleOps with StructExpOpt {
  case class ETuple2[A:Manifest,B:Manifest](_1: Exp[A],_2: Exp[B]) extends Def[(A,B)] {
    val m1 = manifest[A]
    val m2 = manifest[B]
  }
  case class ETuple3[A:Manifest,B:Manifest,C:Manifest](_1: Exp[A],_2: Exp[B],_3: Exp[C]) extends Def[(A,B,C)] {
    val m1 = manifest[A]
    val m2 = manifest[B]
    val m3 = manifest[C]
  }
  case class ETuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](_1: Exp[A],_2: Exp[B],_3: Exp[C],_4: Exp[D]) extends Def[(A,B,C,D)] {
    val m1 = manifest[A]
    val m2 = manifest[B]
    val m3 = manifest[C]
    val m4 = manifest[D]
  }
  case class ETuple5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](_1: Exp[A],_2: Exp[B],_3: Exp[C],_4: Exp[D],_5: Exp[E]) extends Def[(A,B,C,D,E)] {
    val m1 = manifest[A]
    val m2 = manifest[B]
    val m3 = manifest[C]
    val m4 = manifest[D]
    val m5 = manifest[E]
  }

  val tuple_elems: Seq[String] = List("_1", "_2", "_3", "_4", "_5")

  implicit def make_tuple2[A:Manifest,B:Manifest](t: (Exp[A],Exp[B]))(implicit pos: SourceContext) : Exp[(A,B)] = {
    //(ETuple2(t._1, t._2)).atPhase(CCodegenLowering) {
      struct(classTag[(A,B)], tuple_elems(1) -> t._1, tuple_elems(2) -> t._2)
    //}
  }
  implicit def make_tuple3[A:Manifest,B:Manifest,C:Manifest](t: (Exp[A],Exp[B],Exp[C]))(implicit pos: SourceContext) : Exp[(A,B,C)] = struct(classTag[(A,B,C)], tuple_elems(1) -> t._1, tuple_elems(2) -> t._2, tuple_elems(3) -> t._3)
  implicit def make_tuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](t: (Exp[A],Exp[B],Exp[C],Exp[D]))(implicit pos: SourceContext) : Exp[(A,B,C,D)] = struct(classTag[(A,B,C,D)], tuple_elems(1) -> t._1, tuple_elems(2) -> t._2, tuple_elems(3) -> t._3, tuple_elems(4) -> t._4)
  implicit def make_tuple5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](t: (Exp[A],Exp[B],Exp[C],Exp[D],Exp[E]))(implicit pos: SourceContext) : Exp[(A,B,C,D,E)] = struct(classTag[(A,B,C,D,E)], tuple_elems(1) -> t._1, tuple_elems(2) -> t._2, tuple_elems(3) -> t._3, tuple_elems(4) -> t._4, tuple_elems(5) -> t._5)

  case class Tuple2Access1[A:Manifest](t: Exp[(A,_)]) extends Def[A] { val m = manifest[A] }
  case class Tuple2Access2[B:Manifest](t: Exp[(_,B)]) extends Def[B] { val m = manifest[B] }
  case class Tuple3Access1[A:Manifest](t: Exp[(A,_,_)]) extends Def[A] { val m = manifest[A] }
  case class Tuple3Access2[B:Manifest](t: Exp[(_,B,_)]) extends Def[B] { val m = manifest[B] }
  case class Tuple3Access3[C:Manifest](t: Exp[(_,_,C)]) extends Def[C] { val m = manifest[C] }
  case class Tuple4Access1[A:Manifest](t: Exp[(A,_,_,_)]) extends Def[A] { val m = manifest[A] }
  case class Tuple4Access2[B:Manifest](t: Exp[(_,B,_,_)]) extends Def[B] { val m = manifest[B] }
  case class Tuple4Access3[C:Manifest](t: Exp[(_,_,C,_)]) extends Def[C] { val m = manifest[C] }
  case class Tuple4Access4[D:Manifest](t: Exp[(_,_,_,D)]) extends Def[D] { val m = manifest[D] }
  case class Tuple5Access1[A:Manifest](t: Exp[(A,_,_,_,_)]) extends Def[A] { val m = manifest[A] }
  case class Tuple5Access2[B:Manifest](t: Exp[(_,B,_,_,_)]) extends Def[B] { val m = manifest[B] }
  case class Tuple5Access3[C:Manifest](t: Exp[(_,_,C,_,_)]) extends Def[C] { val m = manifest[C] }
  case class Tuple5Access4[D:Manifest](t: Exp[(_,_,_,D,_)]) extends Def[D] { val m = manifest[D] }
  case class Tuple5Access5[E:Manifest](t: Exp[(_,_,_,_,E)]) extends Def[E] { val m = manifest[E] }

  def tuple2_get1[A:Manifest](t: Exp[(A,_)])(implicit pos: SourceContext) = field[A](t, tuple_elems(1))
  def tuple2_get2[B:Manifest](t: Exp[(_,B)])(implicit pos: SourceContext) = field[B](t, tuple_elems(2))

  def tuple3_get1[A:Manifest](t: Exp[(A,_,_)])(implicit pos: SourceContext) = field[A](t, tuple_elems(1))
  def tuple3_get2[B:Manifest](t: Exp[(_,B,_)])(implicit pos: SourceContext) = field[B](t, tuple_elems(2))
  def tuple3_get3[C:Manifest](t: Exp[(_,_,C)])(implicit pos: SourceContext) = field[C](t, tuple_elems(3))

  def tuple4_get1[A:Manifest](t: Exp[(A,_,_,_)])(implicit pos: SourceContext) = field[A](t, tuple_elems(1))
  def tuple4_get2[B:Manifest](t: Exp[(_,B,_,_)])(implicit pos: SourceContext) = field[B](t, tuple_elems(2))
  def tuple4_get3[C:Manifest](t: Exp[(_,_,C,_)])(implicit pos: SourceContext) = field[C](t, tuple_elems(3))
  def tuple4_get4[D:Manifest](t: Exp[(_,_,_,D)])(implicit pos: SourceContext) = field[D](t, tuple_elems(4))

  def tuple5_get1[A:Manifest](t: Exp[(A,_,_,_,_)])(implicit pos: SourceContext) = field[A](t, tuple_elems(1))
  def tuple5_get2[B:Manifest](t: Exp[(_,B,_,_,_)])(implicit pos: SourceContext) = field[B](t, tuple_elems(2))
  def tuple5_get3[C:Manifest](t: Exp[(_,_,C,_,_)])(implicit pos: SourceContext) = field[C](t, tuple_elems(3))
  def tuple5_get4[D:Manifest](t: Exp[(_,_,_,D,_)])(implicit pos: SourceContext) = field[D](t, tuple_elems(4))
  def tuple5_get5[E:Manifest](t: Exp[(_,_,_,_,E)])(implicit pos: SourceContext) = field[E](t, tuple_elems(5))

  object Both { def unapply[T](x:T):Some[(T,T)] = Some((x,x)) }
}

trait TupleGenBase extends GenericCodegen with BaseGenStruct {
  val IR: TupleOpsExp

  override def remap[A](m: Manifest[A]) = m.runtimeClass.getSimpleName match {
    case "Tuple2" =>
      val elems = IR.tuple_elems.take(2) zip m.typeArguments
      IR.registerStructType(IR.structName(m), elems)
      IR.structName(m)
    case "Tuple3" =>
      val elems = IR.tuple_elems.take(3) zip m.typeArguments
      IR.registerStructType(IR.structName(m), elems)
      IR.structName(m)
    case "Tuple4" =>
      val elems = IR.tuple_elems.take(4) zip m.typeArguments
      IR.registerStructType(IR.structName(m), elems)
      IR.structName(m)
    case "Tuple5" =>
      val elems = IR.tuple_elems.take(5) zip m.typeArguments
      IR.registerStructType(IR.structName(m), elems)
      IR.structName(m)
    case _ => super.remap(m)
  }
}

trait ScalaGenTupleOps extends ScalaGenBase with TupleGenBase with ScalaGenStruct { val IR: TupleOpsExp }
trait CGenTupleOps extends CGenBase with TupleGenBase with CGenStruct { val IR: TupleOpsExp }
