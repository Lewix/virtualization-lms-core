package scala.virtualization.lms
package epfl
package test17

import common._
import internal.CNestedCodegen

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}


class TestCCollections extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test17-"
  
  trait DSL extends Structs with TupleOps with ArrayOps with COpsPkg with UncheckedOps with LiftPrimitives with LiftString with LiftVariables {
    // keep track of top level functions
    case class TopLevel[A,B](name: String, mA: Manifest[A], mB:Manifest[B], f: Rep[A] => Rep[B])
    val rec = new scala.collection.mutable.HashMap[String,TopLevel[_,_]]
    def toplevel[A:Manifest,B:Manifest](name: String)(f: Rep[A] => Rep[B]): Rep[A] => Rep[B] = {
      val g = (x: Rep[A]) => unchecked[B](name,"(",x,")")
      rec.getOrElseUpdate(name, TopLevel(name, manifest[A], manifest[B], f))
      g
    }
  }

  trait Impl extends DSL with StructExp with ArrayOpsExp with COpsPkgExp with TupleOpsExp with UncheckedOpsExp { self => 
    val codegen = new CCodeGenPkg with CGenStruct with CGenTupleOps with CGenArrayOps with CGenVariables with CGenUncheckedOps with CNestedCodegen { val IR: self.type = self }
    def emitAll(): Unit = {
      assert(codegen ne null) //careful about initialization order
      rec.foreach { case (k,x) =>
        val stream = new PrintWriter(System.out)
        stream.println("/* FILE: " + x.name + ".c */")
        for ((_,v) <- rec) codegen.emitForwardDef(mtype(v.mA)::Nil, v.name, stream)(mtype(v.mB))
        codegen.emitSource1(x.f, x.name, stream)(mtype(x.mA), mtype(x.mB))
      }
    }
    emitAll()
  }

  def testCTuplesCreation = {
    withOutFile(prefix+"ctuples1") {
      trait Prog extends DSL {
        toplevel("main") { x: Rep[Int] =>
          make_tuple2((x, x+1))
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"ctuples1")
  }
  
  def testCTuplesAccess = {
    withOutFile(prefix+"ctuples2") {
      trait Prog extends DSL {
        toplevel("main") { x: Rep[(Int,Int)] =>
          x._1 + x._2
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"ctuples2")
  }

  def testCArrayCreation = {
    withOutFile(prefix+"carrays1") {
      trait Prog extends DSL {
        toplevel("main") { x: Rep[Int] =>
          val a = array_obj_new[Int](unit(5))
          array_update(a, 1, array_length(a))
          a
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"carrays1")
  }
}

