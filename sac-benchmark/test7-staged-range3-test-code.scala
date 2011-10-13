package scala.virtualization.lms
package epfl
package test7
package original

import test7.original.Conversions._
import test7.original.Operations._
import test7.original.SpecificOperations._

/*****************************************
  Emitting Generated Code                  
*******************************************/
class Experiment extends ((scala.virtualization.lms.epfl.test7.original.MDArray[Double])=>(scala.virtualization.lms.epfl.test7.original.MDArray[Double])) {
  def apply(x181:scala.virtualization.lms.epfl.test7.original.MDArray[Double]): scala.virtualization.lms.epfl.test7.original.MDArray[Double] = {
    // RuntimeCheck : POST:   S181 = [u396  u395  u394]                              from Bubble up shape for Sym(181) <- Shape(Sym(181))
    // Shape: V182=[u396  u395  u394] and S182=[3]
    val x182: MDArray[Int] = shape(x181)
    // RuntimeCheck : POST:   V182 = [u396  u395  u394]                              from Bubble up value for Sym(182) <- GenArrayWith(Sym(182) - Sym(188))
    // RuntimeCheck : POST:   S182 = [3]                                             from Bubble up shape for Sym(182) <- GenArrayWith(Sym(182) - Sym(188))
    // RuntimeCheck : POST:   V188 = [u327]                                          from Bubble up value for Sym(188) <- GenArrayWith(Sym(182) - Sym(188))
    // RuntimeCheck : POST:   S188 = []                                              from Bubble up shape for Sym(188) <- GenArrayWith(Sym(182) - Sym(188))
    // RuntimeCheck : PRE:    S182 = [u16097]                                        from GenArrayWith(Sym(182) - Sym(188))
    // RuntimeCheck : PRE:    S182 = S2                                              from GenArrayWith(Sym(182) - Sym(188))
    // RuntimeCheck : PRE:    V182(:length(V2)) < V2                                 from GenArrayWith(Sym(182) - Sym(188))
    // RuntimeCheck : PRE:    V182(length(V2):) = S188                               from GenArrayWith(Sym(182) - Sym(188))
    // Shape: S189=[u396  u395  u394]
    
    val x189: MDArray[Boolean] = {
      val opName: String = "genarray"
      var result: Array[Boolean] = null
      var rshape: Array[Int] = null
      // Shape: V2=[1  0  0] and S2=[3]
      val x2: MDArray[Int] = internalReshape(3::Nil, Array(1, 0, 0), "knownAtCompileTime")
      // Shape: V3=[2  1  1] and S3=[3]
      val x3: MDArray[Int] = internalReshape(3::Nil, Array(2, 1, 1), "knownAtCompileTime")
      // Shape: V4=[u2] and S4=[]
      val x4: Boolean = internalReshape(Nil, Array(false), "knownAtCompileTime")
      // RuntimeCheck : POST:   V4 = [u2]                                              from Bubble up value for Sym(4) <- ToValue(Sym(4))
      // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- ToValue(Sym(4))
      // RuntimeCheck : PRE:    length(S4) = length([])                                from ToValue(Sym(4))
      // Shape: V5=[u238] and S5=[]
      val x5: Boolean = x4
      // RuntimeCheck : POST:   V5 = [u238]                                            from Bubble up value for Sym(5) <- FromValue(Sym(5))
      // RuntimeCheck : POST:   S5 = []                                                from Bubble up shape for Sym(5) <- FromValue(Sym(5))
      // Shape: V14=[u237] and S14=[]
      val x14: Boolean = x5
      // RuntimeCheck : POST:   V14 = [u237]                                           from Bubble up value for Sym(14) <- ToValue(Sym(14))
      // RuntimeCheck : POST:   S14 = []                                               from Bubble up shape for Sym(14) <- ToValue(Sym(14))
      // RuntimeCheck : PRE:    length(S14) = length([])                               from ToValue(Sym(14))
      // Shape: V15=[u236] and S15=[]
      val x15: Boolean = x14
      // Shape: V17=[u3] and S17=[]
      val x17: Boolean = internalReshape(Nil, Array(true), "knownAtCompileTime")
      // Shape: V7=[1] and S7=[]
      val x7: Int = internalReshape(Nil, Array(1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(182) and Sym(7))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(182) and Sym(7))
      // RuntimeCheck : POST:   V182 = [u396  u395  u394]                              from Bubble up value for Sym(182) <- InfixOp(-: Sym(182) and Sym(7))
      // RuntimeCheck : POST:   S182 = [3]                                             from Bubble up shape for Sym(182) <- InfixOp(-: Sym(182) and Sym(7))
      // RuntimeCheck : PRE:    S182 = S7 OR S7 = []                                   from InfixOp(-: Sym(182) and Sym(7))
      // Shape: V183=[u449  u448  u447] and S183=[3]
      val x183: MDArray[Int] = {
        val result = new Array[Int](shape(x182).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x182.content()(i) -  x7
        internalReshape(shape(x182), result, "infixOpAA")
      }
      // Shape: V12=[0] and S12=[]
      val x12: Int = internalReshape(Nil, Array(0), "knownAtCompileTime")
      // Shape: V9=[0] and S9=[1]
      val x9: MDArray[Int] = internalReshape(1::Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V182 = [u396  u395  u394]                              from Bubble up value for Sym(182) <- Shape(Sym(182))
      // RuntimeCheck : POST:   S182 = [3]                                             from Bubble up shape for Sym(182) <- Shape(Sym(182))
      // Shape: V184=[3] and S184=[1]
      val x184: MDArray[Int] = shape(x182)
      // RuntimeCheck : POST:   V184 = [3]                                             from Bubble up value for Sym(184) <- Sel(Sym(9), Sym(184))
      // RuntimeCheck : POST:   S184 = [1]                                             from Bubble up shape for Sym(184) <- Sel(Sym(9), Sym(184))
      // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(184))
      // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(184))
      // RuntimeCheck : PRE:    length(S9) = length([u16134])                          from Sel(Sym(9), Sym(184))
      // RuntimeCheck : PRE:    S184(:length(V9)) < V9                                 from Sel(Sym(9), Sym(184))
      // Shape: V185=[3] and S185=[]
      
      // Shape: V185=[3] and S185=[]
      val x185: Int = x184.content()(flatten(shape(x184), x9, "sel"))
      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(185))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(185))
      // RuntimeCheck : POST:   V185 = [3]                                             from Bubble up value for Sym(185) <- Values(Sym(12), Sym(185))
      // RuntimeCheck : POST:   S185 = []                                              from Bubble up shape for Sym(185) <- Values(Sym(12), Sym(185))
      // RuntimeCheck : PRE:    S185 = []                                              from Values(Sym(12), Sym(185))
      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(185))
      // Shape: V186=[0  0  0] and S186=[3]
      val x186: MDArray[Int] = {
        val result = new Array[Int](x185)
        for(i <- List.range(0, result.length))
        result(i) = x12
        internalReshape(x185::Nil, result, "values")
      }
      // RuntimeCheck : POST:   V17 = [u3]                                             from Bubble up value for Sym(17) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : POST:   S17 = []                                               from Bubble up shape for Sym(17) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : POST:   S187 = [3]                                             from Bubble up shape for Sym(187) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : POST:   V186 = [0  0  0]                                       from Bubble up value for Sym(186) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : POST:   S186 = [3]                                             from Bubble up shape for Sym(186) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : POST:   V3 = [2  1  1]                                         from Bubble up value for Sym(3) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : POST:   S3 = [3]                                               from Bubble up shape for Sym(3) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : POST:   V15 = [u236]                                           from Bubble up value for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : POST:   V183 = [u449  u448  u447]                              from Bubble up value for Sym(183) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : POST:   S183 = [3]                                             from Bubble up shape for Sym(183) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : POST:   V15 = [u236]                                           from Bubble up value for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : POST:   V2 = [1  0  0]                                         from Bubble up value for Sym(2) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : POST:   S2 = [3]                                               from Bubble up shape for Sym(2) <- With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : PRE:    length(S2) = length([u16098])                          from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : PRE:    S183 = S2                                              from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : PRE:    S3 = S2                                                from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : PRE:    S186 = S2                                              from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // RuntimeCheck : PRE:    V2 < V183                                              from With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      // Shape: V188=[u327] and S188=[]
      // with: With(lb=Sym(2) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(183) step=Sym(3) width=Sym(186)  Sym(187) => Sym(17))
      val lb0: Int = x2.content()(0)
      val ub0: Int = x183.content()(0)
      val step0: Int = x3.content()(0)
      val width0: Int = x186.content()(0)
      val ll0: Int = if (x15) lb0 + 1 else lb0
      val ul0: Int = if (x15) ub0 else ub0 + 1
      for (iv0 <- List.range(ll0, ul0)) {
        if ((iv0 - lb0) % step0 <= width0) {
          val lb1: Int = x2.content()(1)
          val ub1: Int = x183.content()(1)
          val step1: Int = x3.content()(1)
          val width1: Int = x186.content()(1)
          val ll1: Int = if (x15) lb1 + 1 else lb1
          val ul1: Int = if (x15) ub1 else ub1 + 1
          for (iv1 <- List.range(ll1, ul1)) {
            if ((iv1 - lb1) % step1 <= width1) {
              val lb2: Int = x2.content()(2)
              val ub2: Int = x183.content()(2)
              val step2: Int = x3.content()(2)
              val width2: Int = x186.content()(2)
              val ll2: Int = if (x15) lb2 + 1 else lb2
              val ul2: Int = if (x15) ub2 else ub2 + 1
              for (iv2 <- List.range(ll2, ul2)) {
                if ((iv2 - lb2) % step2 <= width2) {
                  val x187: MDArray[Int] = iv0::iv1::iv2::Nil
                  val iv: MDArray[Int] = x187
                  val feval: MDArray[Boolean] = {
                    x17
                  }
                  // the action of this loop:
                  if (result == null) {
                    // create the array and shape
                    result = new Array[Boolean](x182.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                    rshape = shape(feval).content()
                  } else {
                    // check shape -- this WILL be redundant due to runtime checks
                    if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                  }
                  // copy new content
                  val mainIndex: Int = flatten(x182 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
                  for (innerIndex <- List.range(0, rshape.length)) {
                    result(mainIndex + innerIndex) = feval(innerIndex)
                  }
                } // if ((iv0 ...
              } // for (iv0 ...
            } // if ((iv1 ...
          } // for (iv1 ...
        } // if ((iv2 ...
      } // for (iv2 ...
      internalReshape(x182 ::: rshape.toList, result, opName)
    }
    
    // RuntimeCheck : POST:   S181 = [u396  u395  u394]                              from Bubble up shape for Sym(181) <- ModArrayWith(Sym(181) - Sym(226))
    // RuntimeCheck : POST:   V226 = [u476]                                          from Bubble up value for Sym(226) <- ModArrayWith(Sym(181) - Sym(226))
    // RuntimeCheck : POST:   S226 = []                                              from Bubble up shape for Sym(226) <- ModArrayWith(Sym(181) - Sym(226))
    // RuntimeCheck : PRE:    S186 = [u16107]                                        from ModArrayWith(Sym(181) - Sym(226))
    // RuntimeCheck : PRE:    S186 = [LengthOf(S181)]                                from ModArrayWith(Sym(181) - Sym(226))
    // RuntimeCheck : PRE:    S181(:length(V186)) < V186                             from ModArrayWith(Sym(181) - Sym(226))
    // RuntimeCheck : PRE:    S181(length(V186):) = S226                             from ModArrayWith(Sym(181) - Sym(226))
    // Shape: S227=[u396  u395  u394]
    
    val x227: MDArray[Double] = {
      val opName: String = "modarray"
      var result: Array[Double] = new Array[Double](shape(x181).content().foldLeft(1)((a,b) => a*b))
      for (i <- List.range(0, result.length)) {
        result(i) = x181.content()(i)
      }
      var rshape: List[Int] = null
      // Shape: V12=[0] and S12=[]
      val x12: Int = internalReshape(Nil, Array(0), "knownAtCompileTime")
      // Shape: V9=[0] and S9=[1]
      val x9: MDArray[Int] = internalReshape(1::Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   V182 = [u396  u395  u394]                              from Bubble up value for Sym(182) <- Shape(Sym(182))
      // RuntimeCheck : POST:   S182 = [3]                                             from Bubble up shape for Sym(182) <- Shape(Sym(182))
      // Shape: V184=[3] and S184=[1]
      val x184: MDArray[Int] = shape(x182)
      // RuntimeCheck : POST:   V184 = [3]                                             from Bubble up value for Sym(184) <- Sel(Sym(9), Sym(184))
      // RuntimeCheck : POST:   S184 = [1]                                             from Bubble up shape for Sym(184) <- Sel(Sym(9), Sym(184))
      // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(184))
      // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(184))
      // RuntimeCheck : PRE:    length(S9) = length([u16134])                          from Sel(Sym(9), Sym(184))
      // RuntimeCheck : PRE:    S184(:length(V9)) < V9                                 from Sel(Sym(9), Sym(184))
      // Shape: V185=[3] and S185=[]
      
      // Shape: V185=[3] and S185=[]
      val x185: Int = x184.content()(flatten(shape(x184), x9, "sel"))
      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(185))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(185))
      // RuntimeCheck : POST:   V185 = [3]                                             from Bubble up value for Sym(185) <- Values(Sym(12), Sym(185))
      // RuntimeCheck : POST:   S185 = []                                              from Bubble up shape for Sym(185) <- Values(Sym(12), Sym(185))
      // RuntimeCheck : PRE:    S185 = []                                              from Values(Sym(12), Sym(185))
      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(185))
      // Shape: V186=[0  0  0] and S186=[3]
      val x186: MDArray[Int] = {
        val result = new Array[Int](x185)
        for(i <- List.range(0, result.length))
        result(i) = x12
        internalReshape(x185::Nil, result, "values")
      }
      // Shape: V17=[u3] and S17=[]
      val x17: Boolean = internalReshape(Nil, Array(true), "knownAtCompileTime")
      // RuntimeCheck : POST:   V17 = [u3]                                             from Bubble up value for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : POST:   S17 = []                                               from Bubble up shape for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : PRE:    length(S17) = length([])                               from ToValue(Sym(17))
      // Shape: V21=[u259] and S21=[]
      val x21: Boolean = x17
      // RuntimeCheck : POST:   V21 = [u259]                                           from Bubble up value for Sym(21) <- FromValue(Sym(21))
      // RuntimeCheck : POST:   S21 = []                                               from Bubble up shape for Sym(21) <- FromValue(Sym(21))
      // Shape: V24=[u258] and S24=[]
      val x24: Boolean = x21
      // RuntimeCheck : POST:   V24 = [u258]                                           from Bubble up value for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : POST:   S24 = []                                               from Bubble up shape for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : PRE:    length(S24) = length([])                               from ToValue(Sym(24))
      // Shape: V25=[u257] and S25=[]
      val x25: Boolean = x24
      // Shape: V7=[1] and S7=[]
      val x7: Int = internalReshape(Nil, Array(1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(185))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(185))
      // RuntimeCheck : POST:   V185 = [3]                                             from Bubble up value for Sym(185) <- Values(Sym(7), Sym(185))
      // RuntimeCheck : POST:   S185 = []                                              from Bubble up shape for Sym(185) <- Values(Sym(7), Sym(185))
      // RuntimeCheck : PRE:    S185 = []                                              from Values(Sym(7), Sym(185))
      // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(185))
      // Shape: V192=[1  1  1] and S192=[3]
      val x192: MDArray[Int] = {
        val result = new Array[Int](x185)
        for(i <- List.range(0, result.length))
        result(i) = x7
        internalReshape(x185::Nil, result, "values")
      }
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(182) and Sym(7))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(182) and Sym(7))
      // RuntimeCheck : POST:   V182 = [u396  u395  u394]                              from Bubble up value for Sym(182) <- InfixOp(-: Sym(182) and Sym(7))
      // RuntimeCheck : POST:   S182 = [3]                                             from Bubble up shape for Sym(182) <- InfixOp(-: Sym(182) and Sym(7))
      // RuntimeCheck : PRE:    S182 = S7 OR S7 = []                                   from InfixOp(-: Sym(182) and Sym(7))
      // Shape: V223=[u446  u445  u444] and S223=[3]
      val x223: MDArray[Int] = {
        val result = new Array[Int](shape(x182).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x182.content()(i) -  x7
        internalReshape(shape(x182), result, "infixOpAA")
      }
      // Shape: V49=[u11] and S49=[]
      val x49: Double = internalReshape(Nil, Array(0.16666666666666666), "knownAtCompileTime")
      // Shape: V20=[u12] and S20=[]
      val x20: Double = internalReshape(Nil, Array(0.1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V20 = [u12]                                            from Bubble up value for Sym(20) <- InfixOp(*: Sym(181) and Sym(20))
      // RuntimeCheck : POST:   S20 = []                                               from Bubble up shape for Sym(20) <- InfixOp(*: Sym(181) and Sym(20))
      // RuntimeCheck : POST:   S181 = [u396  u395  u394]                              from Bubble up shape for Sym(181) <- InfixOp(*: Sym(181) and Sym(20))
      // RuntimeCheck : PRE:    S181 = S20 OR S20 = []                                 from InfixOp(*: Sym(181) and Sym(20))
      // Shape: S190=[u396  u395  u394]
      val x190: MDArray[Double] = {
        val result = new Array[Double](shape(x181).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x181.content()(i) *  x20
        internalReshape(shape(x181), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   S181 = [u396  u395  u394]                              from Bubble up shape for Sym(181) <- ModArrayWith(Sym(181) - Sym(219))
      // RuntimeCheck : POST:   V219 = [u271]                                          from Bubble up value for Sym(219) <- ModArrayWith(Sym(181) - Sym(219))
      // RuntimeCheck : POST:   S219 = []                                              from Bubble up shape for Sym(219) <- ModArrayWith(Sym(181) - Sym(219))
      // RuntimeCheck : PRE:    S186 = [u16123]                                        from ModArrayWith(Sym(181) - Sym(219))
      // RuntimeCheck : PRE:    S186 = [LengthOf(S181)]                                from ModArrayWith(Sym(181) - Sym(219))
      // RuntimeCheck : PRE:    S181(:length(V186)) < V186                             from ModArrayWith(Sym(181) - Sym(219))
      // RuntimeCheck : PRE:    S181(length(V186):) = S219                             from ModArrayWith(Sym(181) - Sym(219))
      // Shape: S220=[u396  u395  u394]
      
      val x220: MDArray[Double] = {
        val opName: String = "modarray"
        var result: Array[Double] = new Array[Double](shape(x181).content().foldLeft(1)((a,b) => a*b))
        for (i <- List.range(0, result.length)) {
          result(i) = x181.content()(i)
        }
        var rshape: List[Int] = null
        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(182) and Sym(7))
        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(182) and Sym(7))
        // RuntimeCheck : POST:   V182 = [u396  u395  u394]                              from Bubble up value for Sym(182) <- InfixOp(-: Sym(182) and Sym(7))
        // RuntimeCheck : POST:   S182 = [3]                                             from Bubble up shape for Sym(182) <- InfixOp(-: Sym(182) and Sym(7))
        // RuntimeCheck : PRE:    S182 = S7 OR S7 = []                                   from InfixOp(-: Sym(182) and Sym(7))
        // Shape: V191=[u443  u442  u441] and S191=[3]
        val x191: MDArray[Int] = {
          val result = new Array[Int](shape(x182).content().foldLeft(1)((a,b) => a*b))
          for(i <- List.range(0, result.length))
          result(i) = x182.content()(i) -  x7
          internalReshape(shape(x182), result, "infixOpAA")
        }
        // Shape: V115=[u13] and S115=[]
        val x115: Double = internalReshape(Nil, Array(0.0), "knownAtCompileTime")
        // RuntimeCheck : POST:   V218 = [u272]                                          from Bubble up value for Sym(218) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : POST:   S218 = []                                              from Bubble up shape for Sym(218) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : POST:   S193 = [3]                                             from Bubble up shape for Sym(193) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : POST:   V186 = [0  0  0]                                       from Bubble up value for Sym(186) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : POST:   S186 = [3]                                             from Bubble up shape for Sym(186) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : POST:   V192 = [1  1  1]                                       from Bubble up value for Sym(192) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : POST:   S192 = [3]                                             from Bubble up shape for Sym(192) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : POST:   V25 = [u257]                                           from Bubble up value for Sym(25) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : POST:   V191 = [u443  u442  u441]                              from Bubble up value for Sym(191) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : POST:   S191 = [3]                                             from Bubble up shape for Sym(191) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : POST:   V25 = [u257]                                           from Bubble up value for Sym(25) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : POST:   V186 = [0  0  0]                                       from Bubble up value for Sym(186) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : POST:   S186 = [3]                                             from Bubble up shape for Sym(186) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : PRE:    length(S186) = length([u16124])                        from With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : PRE:    S191 = S186                                            from With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : PRE:    S192 = S186                                            from With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : PRE:    S186 = S186                                            from With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // RuntimeCheck : PRE:    V186 < V191                                            from With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        // Shape: V219=[u271] and S219=[]
        // with: With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(191) step=Sym(192) width=Sym(186)  Sym(193) => Sym(218))
        val lb0: Int = x186.content()(0)
        val ub0: Int = x191.content()(0)
        val step0: Int = x192.content()(0)
        val width0: Int = x186.content()(0)
        val ll0: Int = if (x25) lb0 + 1 else lb0
        val ul0: Int = if (x25) ub0 else ub0 + 1
        for (iv0 <- List.range(ll0, ul0)) {
          if ((iv0 - lb0) % step0 <= width0) {
            val lb1: Int = x186.content()(1)
            val ub1: Int = x191.content()(1)
            val step1: Int = x192.content()(1)
            val width1: Int = x186.content()(1)
            val ll1: Int = if (x25) lb1 + 1 else lb1
            val ul1: Int = if (x25) ub1 else ub1 + 1
            for (iv1 <- List.range(ll1, ul1)) {
              if ((iv1 - lb1) % step1 <= width1) {
                val lb2: Int = x186.content()(2)
                val ub2: Int = x191.content()(2)
                val step2: Int = x192.content()(2)
                val width2: Int = x186.content()(2)
                val ll2: Int = if (x25) lb2 + 1 else lb2
                val ul2: Int = if (x25) ub2 else ub2 + 1
                for (iv2 <- List.range(ll2, ul2)) {
                  if ((iv2 - lb2) % step2 <= width2) {
                    val x193: MDArray[Int] = iv0::iv1::iv2::Nil
                    val iv: MDArray[Int] = x193
                    val feval: MDArray[Double] = {
                      // RuntimeCheck : POST:   V212 = [u275]                                          from Bubble up value for Sym(212) <- FoldArrayWith(Sym(115), fold (Sym(210), Sym(211)) => Sym(212), Sym(215))
                      // RuntimeCheck : POST:   S212 = []                                              from Bubble up shape for Sym(212) <- FoldArrayWith(Sym(115), fold (Sym(210), Sym(211)) => Sym(212), Sym(215))
                      // RuntimeCheck : POST:   S211 = []                                              from Bubble up shape for Sym(211) <- FoldArrayWith(Sym(115), fold (Sym(210), Sym(211)) => Sym(212), Sym(215))
                      // RuntimeCheck : POST:   S210 = []                                              from Bubble up shape for Sym(210) <- FoldArrayWith(Sym(115), fold (Sym(210), Sym(211)) => Sym(212), Sym(215))
                      // RuntimeCheck : POST:   V115 = [u13]                                           from Bubble up value for Sym(115) <- FoldArrayWith(Sym(115), fold (Sym(210), Sym(211)) => Sym(212), Sym(215))
                      // RuntimeCheck : POST:   S115 = []                                              from Bubble up shape for Sym(115) <- FoldArrayWith(Sym(115), fold (Sym(210), Sym(211)) => Sym(212), Sym(215))
                      // RuntimeCheck : POST:   V215 = [u490]                                          from Bubble up value for Sym(215) <- FoldArrayWith(Sym(115), fold (Sym(210), Sym(211)) => Sym(212), Sym(215))
                      // RuntimeCheck : POST:   S215 = []                                              from Bubble up shape for Sym(215) <- FoldArrayWith(Sym(115), fold (Sym(210), Sym(211)) => Sym(212), Sym(215))
                      // RuntimeCheck : PRE:    S115 = S215                                            from FoldArrayWith(Sym(115), fold (Sym(210), Sym(211)) => Sym(212), Sym(215))
                      // RuntimeCheck : PRE:    S212 = S215                                            from FoldArrayWith(Sym(115), fold (Sym(210), Sym(211)) => Sym(212), Sym(215))
                      // Shape: V216=[u274] and S216=[]
                      
                      val x216: Double = {
                        val opName: String = "fold"
                        var result: MDArray[Double] = x115
                        val foldFunction: (MDArray[Double], MDArray[Double]) => MDArray[Double] = (x210, x211) => {
                          // RuntimeCheck : POST:   S211 = []                                              from Bubble up shape for Sym(211) <- ScalarOperator Sym(210) + Sym(211)
                          // RuntimeCheck : POST:   S210 = []                                              from Bubble up shape for Sym(210) <- ScalarOperator Sym(210) + Sym(211)
                          // RuntimeCheck : PRE:    S210 = []                                              from ScalarOperator Sym(210) + Sym(211)
                          // RuntimeCheck : PRE:    S211 = []                                              from ScalarOperator Sym(210) + Sym(211)
                          // Shape: V212=[u275] and S212=[]
                          val x212: Double = ((a: Double, b: Double) => a + b)(x210, x211)
                          x212
                        }
                        // Shape: V4=[u2] and S4=[]
                        val x4: Boolean = internalReshape(Nil, Array(false), "knownAtCompileTime")
                        // RuntimeCheck : POST:   V4 = [u2]                                              from Bubble up value for Sym(4) <- ToValue(Sym(4))
                        // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- ToValue(Sym(4))
                        // RuntimeCheck : PRE:    length(S4) = length([])                                from ToValue(Sym(4))
                        // Shape: V5=[u238] and S5=[]
                        val x5: Boolean = x4
                        // RuntimeCheck : POST:   V5 = [u238]                                            from Bubble up value for Sym(5) <- FromValue(Sym(5))
                        // RuntimeCheck : POST:   S5 = []                                                from Bubble up shape for Sym(5) <- FromValue(Sym(5))
                        // Shape: V14=[u237] and S14=[]
                        val x14: Boolean = x5
                        // RuntimeCheck : POST:   V14 = [u237]                                           from Bubble up value for Sym(14) <- ToValue(Sym(14))
                        // RuntimeCheck : POST:   S14 = []                                               from Bubble up shape for Sym(14) <- ToValue(Sym(14))
                        // RuntimeCheck : PRE:    length(S14) = length([])                               from ToValue(Sym(14))
                        // Shape: V15=[u236] and S15=[]
                        val x15: Boolean = x14
                        // Shape: V96=[3  3  3] and S96=[3]
                        val x96: MDArray[Int] = internalReshape(3::Nil, Array(3, 3, 3), "knownAtCompileTime")
                        // Shape: V97=[u15  u16  u17  u1 ... 38  u39  u40  u41] and S97=[27]
                        val x97: MDArray[Double] = internalReshape(27::Nil, Array(0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0), "knownAtCompileTime")
                        // RuntimeCheck : POST:   V97 = [u15  u16  u17  u18  u19  u20  u21  u22  u23  u24  u25  u26  u27  u28  u29  u30  u31  u32  u33  u34  u35  u36  u37  u38  u39  u40  u41]     from Bubble up value for Sym(97) <- Reshape(Sym(96), Sym(97))
                        // RuntimeCheck : POST:   S97 = [27]                                             from Bubble up shape for Sym(97) <- Reshape(Sym(96), Sym(97))
                        // RuntimeCheck : POST:   V96 = [3  3  3]                                        from Bubble up value for Sym(96) <- Reshape(Sym(96), Sym(97))
                        // RuntimeCheck : POST:   S96 = [3]                                              from Bubble up shape for Sym(96) <- Reshape(Sym(96), Sym(97))
                        // RuntimeCheck : PRE:    length(S96) = length([u17041])                         from Reshape(Sym(96), Sym(97))
                        // RuntimeCheck : PRE:    prod(V96) = prod(S97)                                  from Reshape(Sym(96), Sym(97))
                        // Shape: V98=[u235  u234  u233  ...  u211  u210  u209] and S98=[3  3  3]
                        val x98: MDArray[Double] = reshape(x96, x97)
                        // RuntimeCheck : POST:   V98 = [u235  u234  u233  u232  u231  u230  u229  u228  u227  u226  u225  u224  u223  u222  u221  u220  u219  u218  u217  u216  u215  u214  u213  u212  u211  u210  u209]     from Bubble up value for Sym(98) <- Shape(Sym(98))
                        // RuntimeCheck : POST:   S98 = [3  3  3]                                        from Bubble up shape for Sym(98) <- Shape(Sym(98))
                        // Shape: V102=[3  3  3] and S102=[3]
                        val x102: MDArray[Int] = shape(x98)
                        // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- GenArrayWith(Sym(102) - Sym(199))
                        // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- GenArrayWith(Sym(102) - Sym(199))
                        // RuntimeCheck : POST:   V199 = [u439]                                          from Bubble up value for Sym(199) <- GenArrayWith(Sym(102) - Sym(199))
                        // RuntimeCheck : POST:   S199 = []                                              from Bubble up shape for Sym(199) <- GenArrayWith(Sym(102) - Sym(199))
                        // RuntimeCheck : PRE:    S102 = [u16913]                                        from GenArrayWith(Sym(102) - Sym(199))
                        // RuntimeCheck : PRE:    S102 = S106                                            from GenArrayWith(Sym(102) - Sym(199))
                        // RuntimeCheck : PRE:    V102(:length(V106)) < V106                             from GenArrayWith(Sym(102) - Sym(199))
                        // RuntimeCheck : PRE:    V102(length(V106):) = S199                             from GenArrayWith(Sym(102) - Sym(199))
                        // Shape: V200=[u438  u437  u436  ...  u414  u413  u412] and S200=[3  3  3]
                        
                        val x200: MDArray[Double] = {
                          val opName: String = "genarray"
                          var result: Array[Double] = null
                          var rshape: Array[Int] = null
                          // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- Shape(Sym(102))
                          // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- Shape(Sym(102))
                          // Shape: V104=[3] and S104=[1]
                          val x104: MDArray[Int] = shape(x102)
                          // RuntimeCheck : POST:   V104 = [3]                                             from Bubble up value for Sym(104) <- Sel(Sym(9), Sym(104))
                          // RuntimeCheck : POST:   S104 = [1]                                             from Bubble up shape for Sym(104) <- Sel(Sym(9), Sym(104))
                          // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(104))
                          // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(104))
                          // RuntimeCheck : PRE:    length(S9) = length([u17008])                          from Sel(Sym(9), Sym(104))
                          // RuntimeCheck : PRE:    S104(:length(V9)) < V9                                 from Sel(Sym(9), Sym(104))
                          // Shape: V105=[3] and S105=[]
                          
                          // Shape: V105=[3] and S105=[]
                          val x105: Int = x104.content()(flatten(shape(x104), x9, "sel"))
                          // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(105))
                          // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(105))
                          // RuntimeCheck : POST:   V105 = [3]                                             from Bubble up value for Sym(105) <- Values(Sym(12), Sym(105))
                          // RuntimeCheck : POST:   S105 = []                                              from Bubble up shape for Sym(105) <- Values(Sym(12), Sym(105))
                          // RuntimeCheck : PRE:    S105 = []                                              from Values(Sym(12), Sym(105))
                          // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(105))
                          // Shape: V106=[0  0  0] and S106=[3]
                          val x106: MDArray[Int] = {
                            val result = new Array[Int](x105)
                            for(i <- List.range(0, result.length))
                            result(i) = x12
                            internalReshape(x105::Nil, result, "values")
                          }
                          // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(105))
                          // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(105))
                          // RuntimeCheck : POST:   V105 = [3]                                             from Bubble up value for Sym(105) <- Values(Sym(7), Sym(105))
                          // RuntimeCheck : POST:   S105 = []                                              from Bubble up shape for Sym(105) <- Values(Sym(7), Sym(105))
                          // RuntimeCheck : PRE:    S105 = []                                              from Values(Sym(7), Sym(105))
                          // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(105))
                          // Shape: V108=[1  1  1] and S108=[3]
                          val x108: MDArray[Int] = {
                            val result = new Array[Int](x105)
                            for(i <- List.range(0, result.length))
                            result(i) = x7
                            internalReshape(x105::Nil, result, "values")
                          }
                          // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(102) and Sym(7))
                          // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(102) and Sym(7))
                          // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- InfixOp(-: Sym(102) and Sym(7))
                          // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- InfixOp(-: Sym(102) and Sym(7))
                          // RuntimeCheck : PRE:    S102 = S7 OR S7 = []                                   from InfixOp(-: Sym(102) and Sym(7))
                          // Shape: V195=[u317  u316  u315] and S195=[3]
                          val x195: MDArray[Int] = {
                            val result = new Array[Int](shape(x102).content().foldLeft(1)((a,b) => a*b))
                            for(i <- List.range(0, result.length))
                            result(i) = x102.content()(i) -  x7
                            internalReshape(shape(x102), result, "infixOpAA")
                          }
                          // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(193) and Sym(7))
                          // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(193) and Sym(7))
                          // RuntimeCheck : POST:   S193 = [3]                                             from Bubble up shape for Sym(193) <- InfixOp(-: Sym(193) and Sym(7))
                          // RuntimeCheck : PRE:    S193 = S7 OR S7 = []                                   from InfixOp(-: Sym(193) and Sym(7))
                          // Shape: V194=[u469  u468  u467] and S194=[3]
                          val x194: MDArray[Int] = {
                            val result = new Array[Int](shape(x193).content().foldLeft(1)((a,b) => a*b))
                            for(i <- List.range(0, result.length))
                            result(i) = x193.content()(i) -  x7
                            internalReshape(shape(x193), result, "infixOpAA")
                          }
                          // RuntimeCheck : POST:   V198 = [u440]                                          from Bubble up value for Sym(198) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : POST:   S198 = []                                              from Bubble up shape for Sym(198) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : POST:   S196 = [3]                                             from Bubble up shape for Sym(196) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : POST:   V106 = [0  0  0]                                       from Bubble up value for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : POST:   S106 = [3]                                             from Bubble up shape for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : POST:   V108 = [1  1  1]                                       from Bubble up value for Sym(108) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : POST:   S108 = [3]                                             from Bubble up shape for Sym(108) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : POST:   V15 = [u236]                                           from Bubble up value for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : POST:   V195 = [u317  u316  u315]                              from Bubble up value for Sym(195) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : POST:   S195 = [3]                                             from Bubble up shape for Sym(195) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : POST:   V15 = [u236]                                           from Bubble up value for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : POST:   V106 = [0  0  0]                                       from Bubble up value for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : POST:   S106 = [3]                                             from Bubble up shape for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : PRE:    length(S106) = length([u16914])                        from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : PRE:    S195 = S106                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : PRE:    S108 = S106                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : PRE:    S106 = S106                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // RuntimeCheck : PRE:    V106 < V195                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          // Shape: V199=[u439] and S199=[]
                          // with: With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(195) step=Sym(108) width=Sym(106)  Sym(196) => Sym(198))
                          val lb0: Int = x106.content()(0)
                          val ub0: Int = x195.content()(0)
                          val step0: Int = x108.content()(0)
                          val width0: Int = x106.content()(0)
                          val ll0: Int = if (x15) lb0 + 1 else lb0
                          val ul0: Int = if (x15) ub0 else ub0 + 1
                          for (iv0 <- List.range(ll0, ul0)) {
                            if ((iv0 - lb0) % step0 <= width0) {
                              val lb1: Int = x106.content()(1)
                              val ub1: Int = x195.content()(1)
                              val step1: Int = x108.content()(1)
                              val width1: Int = x106.content()(1)
                              val ll1: Int = if (x15) lb1 + 1 else lb1
                              val ul1: Int = if (x15) ub1 else ub1 + 1
                              for (iv1 <- List.range(ll1, ul1)) {
                                if ((iv1 - lb1) % step1 <= width1) {
                                  val lb2: Int = x106.content()(2)
                                  val ub2: Int = x195.content()(2)
                                  val step2: Int = x108.content()(2)
                                  val width2: Int = x106.content()(2)
                                  val ll2: Int = if (x15) lb2 + 1 else lb2
                                  val ul2: Int = if (x15) ub2 else ub2 + 1
                                  for (iv2 <- List.range(ll2, ul2)) {
                                    if ((iv2 - lb2) % step2 <= width2) {
                                      val x196: MDArray[Int] = iv0::iv1::iv2::Nil
                                      val iv: MDArray[Int] = x196
                                      val feval: MDArray[Double] = {
                                        // RuntimeCheck : POST:   V194 = [u469  u468  u467]                              from Bubble up value for Sym(194) <- InfixOp(+: Sym(196) and Sym(194))
                                        // RuntimeCheck : POST:   S194 = [3]                                             from Bubble up shape for Sym(194) <- InfixOp(+: Sym(196) and Sym(194))
                                        // RuntimeCheck : POST:   S196 = [3]                                             from Bubble up shape for Sym(196) <- InfixOp(+: Sym(196) and Sym(194))
                                        // RuntimeCheck : PRE:    S196 = S194 OR S194 = []                               from InfixOp(+: Sym(196) and Sym(194))
                                        // Shape: V197=[u507(<u396)  u508(<u395)  u509(<u394)] and S197=[3]
                                        val x197: MDArray[Int] = {
                                          val result = new Array[Int](shape(x196).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x196.content()(i) +  x194.content()(i)
                                          internalReshape(shape(x196), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   S181 = [u396  u395  u394]                              from Bubble up shape for Sym(181) <- Sel(Sym(197), Sym(181))
                                        // RuntimeCheck : POST:   V197 = [u507(<u396)  u508(<u395)  u509(<u394)]         from Bubble up value for Sym(197) <- Sel(Sym(197), Sym(181))
                                        // RuntimeCheck : POST:   S197 = [3]                                             from Bubble up shape for Sym(197) <- Sel(Sym(197), Sym(181))
                                        // RuntimeCheck : PRE:    length(S197) = length([u17039])                        from Sel(Sym(197), Sym(181))
                                        // RuntimeCheck : PRE:    S181(:length(V197)) < V197                             from Sel(Sym(197), Sym(181))
                                        // Shape: V198=[u440] and S198=[]
                                        
                                        // Shape: V198=[u440] and S198=[]
                                        val x198: Double = x181.content()(flatten(shape(x181), x197, "sel"))
                                        x198
                                      }
                                      // the action of this loop:
                                      if (result == null) {
                                        // create the array and shape
                                        result = new Array[Double](x102.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                                        rshape = shape(feval).content()
                                      } else {
                                        // check shape -- this WILL be redundant due to runtime checks
                                        if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                                      }
                                      // copy new content
                                      val mainIndex: Int = flatten(x102 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
                                      for (innerIndex <- List.range(0, rshape.length)) {
                                        result(mainIndex + innerIndex) = feval(innerIndex)
                                      }
                                    } // if ((iv0 ...
                                  } // for (iv0 ...
                                } // if ((iv1 ...
                              } // for (iv1 ...
                            } // if ((iv2 ...
                          } // for (iv2 ...
                          internalReshape(x102 ::: rshape.toList, result, opName)
                        }
                        
                        // RuntimeCheck : POST:   V200 = [u438  u437  u436  u435  u434  u433  u432  u431  u430  u429  u428  u427  u426  u425  u424  u423  u422  u421  u420  u419  u418  u417  u416  u415  u414  u413  u412]     from Bubble up value for Sym(200) <- InfixOp(*: Sym(98) and Sym(200))
                        // RuntimeCheck : POST:   S200 = [3  3  3]                                       from Bubble up shape for Sym(200) <- InfixOp(*: Sym(98) and Sym(200))
                        // RuntimeCheck : POST:   V98 = [u235  u234  u233  u232  u231  u230  u229  u228  u227  u226  u225  u224  u223  u222  u221  u220  u219  u218  u217  u216  u215  u214  u213  u212  u211  u210  u209]     from Bubble up value for Sym(98) <- InfixOp(*: Sym(98) and Sym(200))
                        // RuntimeCheck : POST:   S98 = [3  3  3]                                        from Bubble up shape for Sym(98) <- InfixOp(*: Sym(98) and Sym(200))
                        // RuntimeCheck : PRE:    S98 = S200 OR S200 = []                                from InfixOp(*: Sym(98) and Sym(200))
                        // Shape: V201=[u312  u311  u310  ...  u288  u287  u286] and S201=[3  3  3]
                        val x201: MDArray[Double] = {
                          val result = new Array[Double](shape(x98).content().foldLeft(1)((a,b) => a*b))
                          for(i <- List.range(0, result.length))
                          result(i) = x98.content()(i) *  x200.content()(i)
                          internalReshape(shape(x98), result, "infixOpAA")
                        }
                        // RuntimeCheck : POST:   V201 = [u312  u311  u310  u309  u308  u307  u306  u305  u304  u303  u302  u301  u300  u299  u298  u297  u296  u295  u294  u293  u292  u291  u290  u289  u288  u287  u286]     from Bubble up value for Sym(201) <- Dim(Sym(201))
                        // RuntimeCheck : POST:   S201 = [3  3  3]                                       from Bubble up shape for Sym(201) <- Dim(Sym(201))
                        // Shape: V202=[3] and S202=[]
                        val x202: Int = dim(x201)
                        // RuntimeCheck : POST:   V202 = [3]                                             from Bubble up value for Sym(202) <- FromValue(Sym(202))
                        // RuntimeCheck : POST:   S202 = []                                              from Bubble up shape for Sym(202) <- FromValue(Sym(202))
                        // Shape: V203=[3] and S203=[]
                        val x203: Int = x202
                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(203))
                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(203))
                        // RuntimeCheck : POST:   V203 = [3]                                             from Bubble up value for Sym(203) <- Values(Sym(12), Sym(203))
                        // RuntimeCheck : POST:   S203 = []                                              from Bubble up shape for Sym(203) <- Values(Sym(12), Sym(203))
                        // RuntimeCheck : PRE:    S203 = []                                              from Values(Sym(12), Sym(203))
                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(203))
                        // Shape: V204=[0  0  0] and S204=[3]
                        val x204: MDArray[Int] = {
                          val result = new Array[Int](x203)
                          for(i <- List.range(0, result.length))
                          result(i) = x12
                          internalReshape(x203::Nil, result, "values")
                        }
                        // RuntimeCheck : POST:   V201 = [u312  u311  u310  u309  u308  u307  u306  u305  u304  u303  u302  u301  u300  u299  u298  u297  u296  u295  u294  u293  u292  u291  u290  u289  u288  u287  u286]     from Bubble up value for Sym(201) <- Shape(Sym(201))
                        // RuntimeCheck : POST:   S201 = [3  3  3]                                       from Bubble up shape for Sym(201) <- Shape(Sym(201))
                        // Shape: V205=[3  3  3] and S205=[3]
                        val x205: MDArray[Int] = shape(x201)
                        // RuntimeCheck : POST:   V204 = [0  0  0]                                       from Bubble up value for Sym(204) <- Shape(Sym(204))
                        // RuntimeCheck : POST:   S204 = [3]                                             from Bubble up shape for Sym(204) <- Shape(Sym(204))
                        // Shape: V206=[3] and S206=[1]
                        val x206: MDArray[Int] = shape(x204)
                        // RuntimeCheck : POST:   V206 = [3]                                             from Bubble up value for Sym(206) <- Sel(Sym(9), Sym(206))
                        // RuntimeCheck : POST:   S206 = [1]                                             from Bubble up shape for Sym(206) <- Sel(Sym(9), Sym(206))
                        // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(206))
                        // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(206))
                        // RuntimeCheck : PRE:    length(S9) = length([u16697])                          from Sel(Sym(9), Sym(206))
                        // RuntimeCheck : PRE:    S206(:length(V9)) < V9                                 from Sel(Sym(9), Sym(206))
                        // Shape: V207=[3] and S207=[]
                        
                        // Shape: V207=[3] and S207=[]
                        val x207: Int = x206.content()(flatten(shape(x206), x9, "sel"))
                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(207))
                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(207))
                        // RuntimeCheck : POST:   V207 = [3]                                             from Bubble up value for Sym(207) <- Values(Sym(7), Sym(207))
                        // RuntimeCheck : POST:   S207 = []                                              from Bubble up shape for Sym(207) <- Values(Sym(7), Sym(207))
                        // RuntimeCheck : PRE:    S207 = []                                              from Values(Sym(7), Sym(207))
                        // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(207))
                        // Shape: V208=[1  1  1] and S208=[3]
                        val x208: MDArray[Int] = {
                          val result = new Array[Int](x207)
                          for(i <- List.range(0, result.length))
                          result(i) = x7
                          internalReshape(x207::Nil, result, "values")
                        }
                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(207))
                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(207))
                        // RuntimeCheck : POST:   V207 = [3]                                             from Bubble up value for Sym(207) <- Values(Sym(12), Sym(207))
                        // RuntimeCheck : POST:   S207 = []                                              from Bubble up shape for Sym(207) <- Values(Sym(12), Sym(207))
                        // RuntimeCheck : PRE:    S207 = []                                              from Values(Sym(12), Sym(207))
                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(207))
                        // Shape: V209=[0  0  0] and S209=[3]
                        val x209: MDArray[Int] = {
                          val result = new Array[Int](x207)
                          for(i <- List.range(0, result.length))
                          result(i) = x12
                          internalReshape(x207::Nil, result, "values")
                        }
                        // RuntimeCheck : POST:   V214 = [u491]                                          from Bubble up value for Sym(214) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   S214 = []                                              from Bubble up shape for Sym(214) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   V213 = [u504(<3)  u505(<3)  u506(<3)]                  from Bubble up value for Sym(213) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   S213 = [3]                                             from Bubble up shape for Sym(213) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   V209 = [0  0  0]                                       from Bubble up value for Sym(209) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   S209 = [3]                                             from Bubble up shape for Sym(209) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   V208 = [1  1  1]                                       from Bubble up value for Sym(208) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   S208 = [3]                                             from Bubble up shape for Sym(208) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   V25 = [u257]                                           from Bubble up value for Sym(25) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   V205 = [3  3  3]                                       from Bubble up value for Sym(205) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   S205 = [3]                                             from Bubble up shape for Sym(205) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   V15 = [u236]                                           from Bubble up value for Sym(15) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   V204 = [0  0  0]                                       from Bubble up value for Sym(204) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : POST:   S204 = [3]                                             from Bubble up shape for Sym(204) <- With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : PRE:    length(S204) = length([u16137])                        from With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : PRE:    S205 = S204                                            from With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : PRE:    S208 = S204                                            from With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : PRE:    S209 = S204                                            from With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // RuntimeCheck : PRE:    V204 < V205                                            from With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        // Shape: V215=[u490] and S215=[]
                        // with: With(lb=Sym(204) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(205) step=Sym(208) width=Sym(209)  Sym(213) => Sym(214))
                        val lb0: Int = x204.content()(0)
                        val ub0: Int = x205.content()(0)
                        val step0: Int = x208.content()(0)
                        val width0: Int = x209.content()(0)
                        val ll0: Int = if (x15) lb0 + 1 else lb0
                        val ul0: Int = if (x25) ub0 else ub0 + 1
                        for (iv0 <- List.range(ll0, ul0)) {
                          if ((iv0 - lb0) % step0 <= width0) {
                            val lb1: Int = x204.content()(1)
                            val ub1: Int = x205.content()(1)
                            val step1: Int = x208.content()(1)
                            val width1: Int = x209.content()(1)
                            val ll1: Int = if (x15) lb1 + 1 else lb1
                            val ul1: Int = if (x25) ub1 else ub1 + 1
                            for (iv1 <- List.range(ll1, ul1)) {
                              if ((iv1 - lb1) % step1 <= width1) {
                                val lb2: Int = x204.content()(2)
                                val ub2: Int = x205.content()(2)
                                val step2: Int = x208.content()(2)
                                val width2: Int = x209.content()(2)
                                val ll2: Int = if (x15) lb2 + 1 else lb2
                                val ul2: Int = if (x25) ub2 else ub2 + 1
                                for (iv2 <- List.range(ll2, ul2)) {
                                  if ((iv2 - lb2) % step2 <= width2) {
                                    val x213: MDArray[Int] = iv0::iv1::iv2::Nil
                                    val iv: MDArray[Int] = x213
                                    val feval: MDArray[Double] = {
                                      // RuntimeCheck : POST:   V201 = [u312  u311  u310  u309  u308  u307  u306  u305  u304  u303  u302  u301  u300  u299  u298  u297  u296  u295  u294  u293  u292  u291  u290  u289  u288  u287  u286]     from Bubble up value for Sym(201) <- Sel(Sym(213), Sym(201))
                                      // RuntimeCheck : POST:   S201 = [3  3  3]                                       from Bubble up shape for Sym(201) <- Sel(Sym(213), Sym(201))
                                      // RuntimeCheck : POST:   V213 = [u504(<3)  u505(<3)  u506(<3)]                  from Bubble up value for Sym(213) <- Sel(Sym(213), Sym(201))
                                      // RuntimeCheck : POST:   S213 = [3]                                             from Bubble up shape for Sym(213) <- Sel(Sym(213), Sym(201))
                                      // RuntimeCheck : PRE:    length(S213) = length([u16884])                        from Sel(Sym(213), Sym(201))
                                      // RuntimeCheck : PRE:    S201(:length(V213)) < V213                             from Sel(Sym(213), Sym(201))
                                      // Shape: V214=[u491] and S214=[]
                                      
                                      // Shape: V214=[u491] and S214=[]
                                      val x214: Double = x201.content()(flatten(shape(x201), x213, "sel"))
                                      x214
                                    }
                                    // the action of this loop:
                                    result = foldFunction(result, feval)
                                  } // if ((iv0 ...
                                } // for (iv0 ...
                              } // if ((iv1 ...
                            } // for (iv1 ...
                          } // if ((iv2 ...
                        } // for (iv2 ...
                        result
                      }
                      
                      // RuntimeCheck : POST:   V216 = [u274]                                          from Bubble up value for Sym(216) <- ToValue(Sym(216))
                      // RuntimeCheck : POST:   S216 = []                                              from Bubble up shape for Sym(216) <- ToValue(Sym(216))
                      // RuntimeCheck : PRE:    length(S216) = length([])                              from ToValue(Sym(216))
                      // Shape: V217=[u273] and S217=[]
                      val x217: Double = x216
                      // RuntimeCheck : POST:   V217 = [u273]                                          from Bubble up value for Sym(217) <- FromValue(Sym(217))
                      // RuntimeCheck : POST:   S217 = []                                              from Bubble up shape for Sym(217) <- FromValue(Sym(217))
                      // Shape: V218=[u272] and S218=[]
                      val x218: Double = x217
                      x218
                    }
                    // the action of this loop:
                    if (rshape == null) {
                      rshape = shape(x181).drop(iv.content().length)
                    }
                    val mainIndex: Int = flatten(shape(x181), iv ::: zeros(dim(x181) - iv.content().length), opName)
                    // check shape -- this WILL be redundant due to runtime checks
                    if (shape(feval).content().toList != rshape) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                    // copy new content
                    for (innerIndex <- List.range(0, feval.content().length)) {
                      result(mainIndex + innerIndex) = feval.content()(innerIndex)
                    }
                  } // if ((iv0 ...
                } // for (iv0 ...
              } // if ((iv1 ...
            } // for (iv1 ...
          } // if ((iv2 ...
        } // for (iv2 ...
        internalReshape(shape(x181) ::: rshape.toList, result, opName)
      }
      
      // RuntimeCheck : POST:   S220 = [u396  u395  u394]                              from Bubble up shape for Sym(220) <- InfixOp(+: Sym(190) and Sym(220))
      // RuntimeCheck : POST:   S190 = [u396  u395  u394]                              from Bubble up shape for Sym(190) <- InfixOp(+: Sym(190) and Sym(220))
      // RuntimeCheck : PRE:    S190 = S220 OR S220 = []                               from InfixOp(+: Sym(190) and Sym(220))
      // Shape: S221=[u396  u395  u394]
      val x221: MDArray[Double] = {
        val result = new Array[Double](shape(x190).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x190.content()(i) +  x220.content()(i)
        internalReshape(shape(x190), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   V49 = [u11]                                            from Bubble up value for Sym(49) <- InfixOp(*: Sym(221) and Sym(49))
      // RuntimeCheck : POST:   S49 = []                                               from Bubble up shape for Sym(49) <- InfixOp(*: Sym(221) and Sym(49))
      // RuntimeCheck : POST:   S221 = [u396  u395  u394]                              from Bubble up shape for Sym(221) <- InfixOp(*: Sym(221) and Sym(49))
      // RuntimeCheck : PRE:    S221 = S49 OR S49 = []                                 from InfixOp(*: Sym(221) and Sym(49))
      // Shape: S222=[u396  u395  u394]
      val x222: MDArray[Double] = {
        val result = new Array[Double](shape(x221).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x221.content()(i) *  x49
        internalReshape(shape(x221), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   V225 = [u477]                                          from Bubble up value for Sym(225) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   S225 = []                                              from Bubble up shape for Sym(225) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   V224 = [u484(<u396)  u485(<u395)  u486(<u394)]         from Bubble up value for Sym(224) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   S224 = [3]                                             from Bubble up shape for Sym(224) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   V186 = [0  0  0]                                       from Bubble up value for Sym(186) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   S186 = [3]                                             from Bubble up shape for Sym(186) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   V192 = [1  1  1]                                       from Bubble up value for Sym(192) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   S192 = [3]                                             from Bubble up shape for Sym(192) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   V25 = [u257]                                           from Bubble up value for Sym(25) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   V223 = [u446  u445  u444]                              from Bubble up value for Sym(223) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   S223 = [3]                                             from Bubble up shape for Sym(223) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   V25 = [u257]                                           from Bubble up value for Sym(25) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   V186 = [0  0  0]                                       from Bubble up value for Sym(186) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : POST:   S186 = [3]                                             from Bubble up shape for Sym(186) <- With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : PRE:    length(S186) = length([u16108])                        from With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : PRE:    S223 = S186                                            from With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : PRE:    S192 = S186                                            from With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : PRE:    S186 = S186                                            from With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // RuntimeCheck : PRE:    V186 < V223                                            from With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      // Shape: V226=[u476] and S226=[]
      // with: With(lb=Sym(186) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(223) step=Sym(192) width=Sym(186)  Sym(224) => Sym(225))
      val lb0: Int = x186.content()(0)
      val ub0: Int = x223.content()(0)
      val step0: Int = x192.content()(0)
      val width0: Int = x186.content()(0)
      val ll0: Int = if (x25) lb0 + 1 else lb0
      val ul0: Int = if (x25) ub0 else ub0 + 1
      for (iv0 <- List.range(ll0, ul0)) {
        if ((iv0 - lb0) % step0 <= width0) {
          val lb1: Int = x186.content()(1)
          val ub1: Int = x223.content()(1)
          val step1: Int = x192.content()(1)
          val width1: Int = x186.content()(1)
          val ll1: Int = if (x25) lb1 + 1 else lb1
          val ul1: Int = if (x25) ub1 else ub1 + 1
          for (iv1 <- List.range(ll1, ul1)) {
            if ((iv1 - lb1) % step1 <= width1) {
              val lb2: Int = x186.content()(2)
              val ub2: Int = x223.content()(2)
              val step2: Int = x192.content()(2)
              val width2: Int = x186.content()(2)
              val ll2: Int = if (x25) lb2 + 1 else lb2
              val ul2: Int = if (x25) ub2 else ub2 + 1
              for (iv2 <- List.range(ll2, ul2)) {
                if ((iv2 - lb2) % step2 <= width2) {
                  val x224: MDArray[Int] = iv0::iv1::iv2::Nil
                  val iv: MDArray[Int] = x224
                  val feval: MDArray[Double] = {
                    // RuntimeCheck : POST:   S222 = [u396  u395  u394]                              from Bubble up shape for Sym(222) <- Sel(Sym(224), Sym(222))
                    // RuntimeCheck : POST:   V224 = [u484(<u396)  u485(<u395)  u486(<u394)]         from Bubble up value for Sym(224) <- Sel(Sym(224), Sym(222))
                    // RuntimeCheck : POST:   S224 = [3]                                             from Bubble up shape for Sym(224) <- Sel(Sym(224), Sym(222))
                    // RuntimeCheck : PRE:    length(S224) = length([u16121])                        from Sel(Sym(224), Sym(222))
                    // RuntimeCheck : PRE:    S222(:length(V224)) < V224                             from Sel(Sym(224), Sym(222))
                    // Shape: V225=[u477] and S225=[]
                    
                    // Shape: V225=[u477] and S225=[]
                    val x225: Double = x222.content()(flatten(shape(x222), x224, "sel"))
                    x225
                  }
                  // the action of this loop:
                  if (rshape == null) {
                    rshape = shape(x181).drop(iv.content().length)
                  }
                  val mainIndex: Int = flatten(shape(x181), iv ::: zeros(dim(x181) - iv.content().length), opName)
                  // check shape -- this WILL be redundant due to runtime checks
                  if (shape(feval).content().toList != rshape) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                  // copy new content
                  for (innerIndex <- List.range(0, feval.content().length)) {
                    result(mainIndex + innerIndex) = feval.content()(innerIndex)
                  }
                } // if ((iv0 ...
              } // for (iv0 ...
            } // if ((iv1 ...
          } // for (iv1 ...
        } // if ((iv2 ...
      } // for (iv2 ...
      internalReshape(shape(x181) ::: rshape.toList, result, opName)
    }
    
    // RuntimeCheck : POST:   S181 = [u396  u395  u394]                              from Bubble up shape for Sym(181) <- Where(Sym(189), Sym(227), Sym(181))
    // RuntimeCheck : POST:   S227 = [u396  u395  u394]                              from Bubble up shape for Sym(227) <- Where(Sym(189), Sym(227), Sym(181))
    // RuntimeCheck : POST:   S189 = [u396  u395  u394]                              from Bubble up shape for Sym(189) <- Where(Sym(189), Sym(227), Sym(181))
    // RuntimeCheck : PRE:    S189 = S227                                            from Where(Sym(189), Sym(227), Sym(181))
    // RuntimeCheck : PRE:    S189 = S181                                            from Where(Sym(189), Sym(227), Sym(181))
    // Shape: S228=[u396  u395  u394]
    val x228: MDArray[Double] = {
      val result = new Array[Double](shape(x227).content().foldLeft(1)((a,b) => a*b))
      for(i <- List.range(0, result.length))
      result(i) = if (x189.content()(i)) x227.content()(i) else x181.content()(i)
      internalReshape(shape(x227), result, "where")
    }
    // RuntimeCheck : POST:   S189 = [u396  u395  u394]                              from Bubble up shape for Sym(189) <- UnaryOp(!: Sym(189))
    // Shape: S229=[u396  u395  u394]
    val x229: MDArray[Boolean] = {
      val result = new Array[Boolean](shape(x189).content().foldLeft(1)((a,b) => a*b))
      for(i <- List.range(0, result.length))
      result(i) = !x189.content()(i)
      internalReshape(shape(x189), result, "unaryOp")
    }
    // RuntimeCheck : POST:   S228 = [u396  u395  u394]                              from Bubble up shape for Sym(228) <- ModArrayWith(Sym(228) - Sym(270))
    // RuntimeCheck : POST:   V270 = [u450]                                          from Bubble up value for Sym(270) <- ModArrayWith(Sym(228) - Sym(270))
    // RuntimeCheck : POST:   S270 = []                                              from Bubble up shape for Sym(270) <- ModArrayWith(Sym(228) - Sym(270))
    // RuntimeCheck : PRE:    S234 = [u523]                                          from ModArrayWith(Sym(228) - Sym(270))
    // RuntimeCheck : PRE:    S234 = [LengthOf(S228)]                                from ModArrayWith(Sym(228) - Sym(270))
    // RuntimeCheck : PRE:    S228(:length(V234)) < V234                             from ModArrayWith(Sym(228) - Sym(270))
    // RuntimeCheck : PRE:    S228(length(V234):) = S270                             from ModArrayWith(Sym(228) - Sym(270))
    // Shape: S271=[u396  u395  u394]
    
    val x271: MDArray[Double] = {
      val opName: String = "modarray"
      var result: Array[Double] = new Array[Double](shape(x228).content().foldLeft(1)((a,b) => a*b))
      for (i <- List.range(0, result.length)) {
        result(i) = x228.content()(i)
      }
      var rshape: List[Int] = null
      // Shape: V17=[u3] and S17=[]
      val x17: Boolean = internalReshape(Nil, Array(true), "knownAtCompileTime")
      // RuntimeCheck : POST:   V17 = [u3]                                             from Bubble up value for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : POST:   S17 = []                                               from Bubble up shape for Sym(17) <- ToValue(Sym(17))
      // RuntimeCheck : PRE:    length(S17) = length([])                               from ToValue(Sym(17))
      // Shape: V21=[u259] and S21=[]
      val x21: Boolean = x17
      // RuntimeCheck : POST:   V21 = [u259]                                           from Bubble up value for Sym(21) <- FromValue(Sym(21))
      // RuntimeCheck : POST:   S21 = []                                               from Bubble up shape for Sym(21) <- FromValue(Sym(21))
      // Shape: V24=[u258] and S24=[]
      val x24: Boolean = x21
      // RuntimeCheck : POST:   V24 = [u258]                                           from Bubble up value for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : POST:   S24 = []                                               from Bubble up shape for Sym(24) <- ToValue(Sym(24))
      // RuntimeCheck : PRE:    length(S24) = length([])                               from ToValue(Sym(24))
      // Shape: V25=[u257] and S25=[]
      val x25: Boolean = x24
      // Shape: V12=[0] and S12=[]
      val x12: Int = internalReshape(Nil, Array(0), "knownAtCompileTime")
      // Shape: V9=[0] and S9=[1]
      val x9: MDArray[Int] = internalReshape(1::Nil, Array(0), "knownAtCompileTime")
      // RuntimeCheck : POST:   S228 = [u396  u395  u394]                              from Bubble up shape for Sym(228) <- Shape(Sym(228))
      // Shape: V231=[u396  u395  u394] and S231=[3]
      val x231: MDArray[Int] = shape(x228)
      // RuntimeCheck : POST:   V231 = [u396  u395  u394]                              from Bubble up value for Sym(231) <- Shape(Sym(231))
      // RuntimeCheck : POST:   S231 = [3]                                             from Bubble up shape for Sym(231) <- Shape(Sym(231))
      // Shape: V232=[3] and S232=[1]
      val x232: MDArray[Int] = shape(x231)
      // RuntimeCheck : POST:   V232 = [3]                                             from Bubble up value for Sym(232) <- Sel(Sym(9), Sym(232))
      // RuntimeCheck : POST:   S232 = [1]                                             from Bubble up shape for Sym(232) <- Sel(Sym(9), Sym(232))
      // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(232))
      // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(232))
      // RuntimeCheck : PRE:    length(S9) = length([u7368])                           from Sel(Sym(9), Sym(232))
      // RuntimeCheck : PRE:    S232(:length(V9)) < V9                                 from Sel(Sym(9), Sym(232))
      // Shape: V233=[3] and S233=[]
      
      // Shape: V233=[3] and S233=[]
      val x233: Int = x232.content()(flatten(shape(x232), x9, "sel"))
      // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(233))
      // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(233))
      // RuntimeCheck : POST:   V233 = [3]                                             from Bubble up value for Sym(233) <- Values(Sym(12), Sym(233))
      // RuntimeCheck : POST:   S233 = []                                              from Bubble up shape for Sym(233) <- Values(Sym(12), Sym(233))
      // RuntimeCheck : PRE:    S233 = []                                              from Values(Sym(12), Sym(233))
      // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(233))
      // Shape: V234=[0  0  0] and S234=[3]
      val x234: MDArray[Int] = {
        val result = new Array[Int](x233)
        for(i <- List.range(0, result.length))
        result(i) = x12
        internalReshape(x233::Nil, result, "values")
      }
      // Shape: V7=[1] and S7=[]
      val x7: Int = internalReshape(Nil, Array(1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(233))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(233))
      // RuntimeCheck : POST:   V233 = [3]                                             from Bubble up value for Sym(233) <- Values(Sym(7), Sym(233))
      // RuntimeCheck : POST:   S233 = []                                              from Bubble up shape for Sym(233) <- Values(Sym(7), Sym(233))
      // RuntimeCheck : PRE:    S233 = []                                              from Values(Sym(7), Sym(233))
      // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(233))
      // Shape: V236=[1  1  1] and S236=[3]
      val x236: MDArray[Int] = {
        val result = new Array[Int](x233)
        for(i <- List.range(0, result.length))
        result(i) = x7
        internalReshape(x233::Nil, result, "values")
      }
      // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(231) and Sym(7))
      // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(231) and Sym(7))
      // RuntimeCheck : POST:   V231 = [u396  u395  u394]                              from Bubble up value for Sym(231) <- InfixOp(-: Sym(231) and Sym(7))
      // RuntimeCheck : POST:   S231 = [3]                                             from Bubble up shape for Sym(231) <- InfixOp(-: Sym(231) and Sym(7))
      // RuntimeCheck : PRE:    S231 = S7 OR S7 = []                                   from InfixOp(-: Sym(231) and Sym(7))
      // Shape: V267=[u405  u404  u403] and S267=[3]
      val x267: MDArray[Int] = {
        val result = new Array[Int](shape(x231).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x231.content()(i) -  x7
        internalReshape(shape(x231), result, "infixOpAA")
      }
      // Shape: V49=[u11] and S49=[]
      val x49: Double = internalReshape(Nil, Array(0.16666666666666666), "knownAtCompileTime")
      // Shape: V20=[u12] and S20=[]
      val x20: Double = internalReshape(Nil, Array(0.1), "knownAtCompileTime")
      // RuntimeCheck : POST:   V20 = [u12]                                            from Bubble up value for Sym(20) <- InfixOp(*: Sym(181) and Sym(20))
      // RuntimeCheck : POST:   S20 = []                                               from Bubble up shape for Sym(20) <- InfixOp(*: Sym(181) and Sym(20))
      // RuntimeCheck : POST:   S181 = [u396  u395  u394]                              from Bubble up shape for Sym(181) <- InfixOp(*: Sym(181) and Sym(20))
      // RuntimeCheck : PRE:    S181 = S20 OR S20 = []                                 from InfixOp(*: Sym(181) and Sym(20))
      // Shape: S230=[u396  u395  u394]
      val x230: MDArray[Double] = {
        val result = new Array[Double](shape(x181).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x181.content()(i) *  x20
        internalReshape(shape(x181), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   S228 = [u396  u395  u394]                              from Bubble up shape for Sym(228) <- ModArrayWith(Sym(228) - Sym(263))
      // RuntimeCheck : POST:   V263 = [u122]                                          from Bubble up value for Sym(263) <- ModArrayWith(Sym(228) - Sym(263))
      // RuntimeCheck : POST:   S263 = []                                              from Bubble up shape for Sym(263) <- ModArrayWith(Sym(228) - Sym(263))
      // RuntimeCheck : PRE:    S234 = [u4435]                                         from ModArrayWith(Sym(228) - Sym(263))
      // RuntimeCheck : PRE:    S234 = [LengthOf(S228)]                                from ModArrayWith(Sym(228) - Sym(263))
      // RuntimeCheck : PRE:    S228(:length(V234)) < V234                             from ModArrayWith(Sym(228) - Sym(263))
      // RuntimeCheck : PRE:    S228(length(V234):) = S263                             from ModArrayWith(Sym(228) - Sym(263))
      // Shape: S264=[u396  u395  u394]
      
      val x264: MDArray[Double] = {
        val opName: String = "modarray"
        var result: Array[Double] = new Array[Double](shape(x228).content().foldLeft(1)((a,b) => a*b))
        for (i <- List.range(0, result.length)) {
          result(i) = x228.content()(i)
        }
        var rshape: List[Int] = null
        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(231) and Sym(7))
        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(231) and Sym(7))
        // RuntimeCheck : POST:   V231 = [u396  u395  u394]                              from Bubble up value for Sym(231) <- InfixOp(-: Sym(231) and Sym(7))
        // RuntimeCheck : POST:   S231 = [3]                                             from Bubble up shape for Sym(231) <- InfixOp(-: Sym(231) and Sym(7))
        // RuntimeCheck : PRE:    S231 = S7 OR S7 = []                                   from InfixOp(-: Sym(231) and Sym(7))
        // Shape: V235=[u402  u401  u400] and S235=[3]
        val x235: MDArray[Int] = {
          val result = new Array[Int](shape(x231).content().foldLeft(1)((a,b) => a*b))
          for(i <- List.range(0, result.length))
          result(i) = x231.content()(i) -  x7
          internalReshape(shape(x231), result, "infixOpAA")
        }
        // Shape: V115=[u13] and S115=[]
        val x115: Double = internalReshape(Nil, Array(0.0), "knownAtCompileTime")
        // RuntimeCheck : POST:   V262 = [u123]                                          from Bubble up value for Sym(262) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : POST:   S262 = []                                              from Bubble up shape for Sym(262) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : POST:   S237 = [3]                                             from Bubble up shape for Sym(237) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : POST:   V234 = [0  0  0]                                       from Bubble up value for Sym(234) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : POST:   S234 = [3]                                             from Bubble up shape for Sym(234) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : POST:   V236 = [1  1  1]                                       from Bubble up value for Sym(236) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : POST:   S236 = [3]                                             from Bubble up shape for Sym(236) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : POST:   V25 = [u257]                                           from Bubble up value for Sym(25) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : POST:   V235 = [u402  u401  u400]                              from Bubble up value for Sym(235) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : POST:   S235 = [3]                                             from Bubble up shape for Sym(235) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : POST:   V25 = [u257]                                           from Bubble up value for Sym(25) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : POST:   V234 = [0  0  0]                                       from Bubble up value for Sym(234) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : POST:   S234 = [3]                                             from Bubble up shape for Sym(234) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : PRE:    length(S234) = length([u4436])                         from With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : PRE:    S235 = S234                                            from With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : PRE:    S236 = S234                                            from With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : PRE:    S234 = S234                                            from With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // RuntimeCheck : PRE:    V234 < V235                                            from With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        // Shape: V263=[u122] and S263=[]
        // with: With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(235) step=Sym(236) width=Sym(234)  Sym(237) => Sym(262))
        val lb0: Int = x234.content()(0)
        val ub0: Int = x235.content()(0)
        val step0: Int = x236.content()(0)
        val width0: Int = x234.content()(0)
        val ll0: Int = if (x25) lb0 + 1 else lb0
        val ul0: Int = if (x25) ub0 else ub0 + 1
        for (iv0 <- List.range(ll0, ul0)) {
          if ((iv0 - lb0) % step0 <= width0) {
            val lb1: Int = x234.content()(1)
            val ub1: Int = x235.content()(1)
            val step1: Int = x236.content()(1)
            val width1: Int = x234.content()(1)
            val ll1: Int = if (x25) lb1 + 1 else lb1
            val ul1: Int = if (x25) ub1 else ub1 + 1
            for (iv1 <- List.range(ll1, ul1)) {
              if ((iv1 - lb1) % step1 <= width1) {
                val lb2: Int = x234.content()(2)
                val ub2: Int = x235.content()(2)
                val step2: Int = x236.content()(2)
                val width2: Int = x234.content()(2)
                val ll2: Int = if (x25) lb2 + 1 else lb2
                val ul2: Int = if (x25) ub2 else ub2 + 1
                for (iv2 <- List.range(ll2, ul2)) {
                  if ((iv2 - lb2) % step2 <= width2) {
                    val x237: MDArray[Int] = iv0::iv1::iv2::Nil
                    val iv: MDArray[Int] = x237
                    val feval: MDArray[Double] = {
                      // RuntimeCheck : POST:   V256 = [u128]                                          from Bubble up value for Sym(256) <- FoldArrayWith(Sym(115), fold (Sym(254), Sym(255)) => Sym(256), Sym(259))
                      // RuntimeCheck : POST:   S256 = []                                              from Bubble up shape for Sym(256) <- FoldArrayWith(Sym(115), fold (Sym(254), Sym(255)) => Sym(256), Sym(259))
                      // RuntimeCheck : POST:   S255 = []                                              from Bubble up shape for Sym(255) <- FoldArrayWith(Sym(115), fold (Sym(254), Sym(255)) => Sym(256), Sym(259))
                      // RuntimeCheck : POST:   S254 = []                                              from Bubble up shape for Sym(254) <- FoldArrayWith(Sym(115), fold (Sym(254), Sym(255)) => Sym(256), Sym(259))
                      // RuntimeCheck : POST:   V115 = [u13]                                           from Bubble up value for Sym(115) <- FoldArrayWith(Sym(115), fold (Sym(254), Sym(255)) => Sym(256), Sym(259))
                      // RuntimeCheck : POST:   S115 = []                                              from Bubble up shape for Sym(115) <- FoldArrayWith(Sym(115), fold (Sym(254), Sym(255)) => Sym(256), Sym(259))
                      // RuntimeCheck : POST:   V259 = [u336]                                          from Bubble up value for Sym(259) <- FoldArrayWith(Sym(115), fold (Sym(254), Sym(255)) => Sym(256), Sym(259))
                      // RuntimeCheck : POST:   S259 = []                                              from Bubble up shape for Sym(259) <- FoldArrayWith(Sym(115), fold (Sym(254), Sym(255)) => Sym(256), Sym(259))
                      // RuntimeCheck : PRE:    S115 = S259                                            from FoldArrayWith(Sym(115), fold (Sym(254), Sym(255)) => Sym(256), Sym(259))
                      // RuntimeCheck : PRE:    S256 = S259                                            from FoldArrayWith(Sym(115), fold (Sym(254), Sym(255)) => Sym(256), Sym(259))
                      // Shape: V260=[u240] and S260=[]
                      
                      val x260: Double = {
                        val opName: String = "fold"
                        var result: MDArray[Double] = x115
                        val foldFunction: (MDArray[Double], MDArray[Double]) => MDArray[Double] = (x254, x255) => {
                          // RuntimeCheck : POST:   S255 = []                                              from Bubble up shape for Sym(255) <- ScalarOperator Sym(254) + Sym(255)
                          // RuntimeCheck : POST:   S254 = []                                              from Bubble up shape for Sym(254) <- ScalarOperator Sym(254) + Sym(255)
                          // RuntimeCheck : PRE:    S254 = []                                              from ScalarOperator Sym(254) + Sym(255)
                          // RuntimeCheck : PRE:    S255 = []                                              from ScalarOperator Sym(254) + Sym(255)
                          // Shape: V256=[u128] and S256=[]
                          val x256: Double = ((a: Double, b: Double) => a + b)(x254, x255)
                          x256
                        }
                        // Shape: V4=[u2] and S4=[]
                        val x4: Boolean = internalReshape(Nil, Array(false), "knownAtCompileTime")
                        // RuntimeCheck : POST:   V4 = [u2]                                              from Bubble up value for Sym(4) <- ToValue(Sym(4))
                        // RuntimeCheck : POST:   S4 = []                                                from Bubble up shape for Sym(4) <- ToValue(Sym(4))
                        // RuntimeCheck : PRE:    length(S4) = length([])                                from ToValue(Sym(4))
                        // Shape: V5=[u238] and S5=[]
                        val x5: Boolean = x4
                        // RuntimeCheck : POST:   V5 = [u238]                                            from Bubble up value for Sym(5) <- FromValue(Sym(5))
                        // RuntimeCheck : POST:   S5 = []                                                from Bubble up shape for Sym(5) <- FromValue(Sym(5))
                        // Shape: V14=[u237] and S14=[]
                        val x14: Boolean = x5
                        // RuntimeCheck : POST:   V14 = [u237]                                           from Bubble up value for Sym(14) <- ToValue(Sym(14))
                        // RuntimeCheck : POST:   S14 = []                                               from Bubble up shape for Sym(14) <- ToValue(Sym(14))
                        // RuntimeCheck : PRE:    length(S14) = length([])                               from ToValue(Sym(14))
                        // Shape: V15=[u236] and S15=[]
                        val x15: Boolean = x14
                        // Shape: V96=[3  3  3] and S96=[3]
                        val x96: MDArray[Int] = internalReshape(3::Nil, Array(3, 3, 3), "knownAtCompileTime")
                        // Shape: V97=[u15  u16  u17  u1 ... 38  u39  u40  u41] and S97=[27]
                        val x97: MDArray[Double] = internalReshape(27::Nil, Array(0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0), "knownAtCompileTime")
                        // RuntimeCheck : POST:   V97 = [u15  u16  u17  u18  u19  u20  u21  u22  u23  u24  u25  u26  u27  u28  u29  u30  u31  u32  u33  u34  u35  u36  u37  u38  u39  u40  u41]     from Bubble up value for Sym(97) <- Reshape(Sym(96), Sym(97))
                        // RuntimeCheck : POST:   S97 = [27]                                             from Bubble up shape for Sym(97) <- Reshape(Sym(96), Sym(97))
                        // RuntimeCheck : POST:   V96 = [3  3  3]                                        from Bubble up value for Sym(96) <- Reshape(Sym(96), Sym(97))
                        // RuntimeCheck : POST:   S96 = [3]                                              from Bubble up shape for Sym(96) <- Reshape(Sym(96), Sym(97))
                        // RuntimeCheck : PRE:    length(S96) = length([u17041])                         from Reshape(Sym(96), Sym(97))
                        // RuntimeCheck : PRE:    prod(V96) = prod(S97)                                  from Reshape(Sym(96), Sym(97))
                        // Shape: V98=[u235  u234  u233  ...  u211  u210  u209] and S98=[3  3  3]
                        val x98: MDArray[Double] = reshape(x96, x97)
                        // RuntimeCheck : POST:   V98 = [u235  u234  u233  u232  u231  u230  u229  u228  u227  u226  u225  u224  u223  u222  u221  u220  u219  u218  u217  u216  u215  u214  u213  u212  u211  u210  u209]     from Bubble up value for Sym(98) <- Shape(Sym(98))
                        // RuntimeCheck : POST:   S98 = [3  3  3]                                        from Bubble up shape for Sym(98) <- Shape(Sym(98))
                        // Shape: V102=[3  3  3] and S102=[3]
                        val x102: MDArray[Int] = shape(x98)
                        // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- GenArrayWith(Sym(102) - Sym(243))
                        // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- GenArrayWith(Sym(102) - Sym(243))
                        // RuntimeCheck : POST:   V243 = [u392]                                          from Bubble up value for Sym(243) <- GenArrayWith(Sym(102) - Sym(243))
                        // RuntimeCheck : POST:   S243 = []                                              from Bubble up shape for Sym(243) <- GenArrayWith(Sym(102) - Sym(243))
                        // RuntimeCheck : PRE:    S102 = [u13017]                                        from GenArrayWith(Sym(102) - Sym(243))
                        // RuntimeCheck : PRE:    S102 = S106                                            from GenArrayWith(Sym(102) - Sym(243))
                        // RuntimeCheck : PRE:    V102(:length(V106)) < V106                             from GenArrayWith(Sym(102) - Sym(243))
                        // RuntimeCheck : PRE:    V102(length(V106):) = S243                             from GenArrayWith(Sym(102) - Sym(243))
                        // Shape: V244=[u391  u390  u389  ...  u367  u366  u365] and S244=[3  3  3]
                        
                        val x244: MDArray[Double] = {
                          val opName: String = "genarray"
                          var result: Array[Double] = null
                          var rshape: Array[Int] = null
                          // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- Shape(Sym(102))
                          // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- Shape(Sym(102))
                          // Shape: V104=[3] and S104=[1]
                          val x104: MDArray[Int] = shape(x102)
                          // RuntimeCheck : POST:   V104 = [3]                                             from Bubble up value for Sym(104) <- Sel(Sym(9), Sym(104))
                          // RuntimeCheck : POST:   S104 = [1]                                             from Bubble up shape for Sym(104) <- Sel(Sym(9), Sym(104))
                          // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(104))
                          // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(104))
                          // RuntimeCheck : PRE:    length(S9) = length([u17008])                          from Sel(Sym(9), Sym(104))
                          // RuntimeCheck : PRE:    S104(:length(V9)) < V9                                 from Sel(Sym(9), Sym(104))
                          // Shape: V105=[3] and S105=[]
                          
                          // Shape: V105=[3] and S105=[]
                          val x105: Int = x104.content()(flatten(shape(x104), x9, "sel"))
                          // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(105))
                          // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(105))
                          // RuntimeCheck : POST:   V105 = [3]                                             from Bubble up value for Sym(105) <- Values(Sym(12), Sym(105))
                          // RuntimeCheck : POST:   S105 = []                                              from Bubble up shape for Sym(105) <- Values(Sym(12), Sym(105))
                          // RuntimeCheck : PRE:    S105 = []                                              from Values(Sym(12), Sym(105))
                          // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(105))
                          // Shape: V106=[0  0  0] and S106=[3]
                          val x106: MDArray[Int] = {
                            val result = new Array[Int](x105)
                            for(i <- List.range(0, result.length))
                            result(i) = x12
                            internalReshape(x105::Nil, result, "values")
                          }
                          // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(105))
                          // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(105))
                          // RuntimeCheck : POST:   V105 = [3]                                             from Bubble up value for Sym(105) <- Values(Sym(7), Sym(105))
                          // RuntimeCheck : POST:   S105 = []                                              from Bubble up shape for Sym(105) <- Values(Sym(7), Sym(105))
                          // RuntimeCheck : PRE:    S105 = []                                              from Values(Sym(7), Sym(105))
                          // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(105))
                          // Shape: V108=[1  1  1] and S108=[3]
                          val x108: MDArray[Int] = {
                            val result = new Array[Int](x105)
                            for(i <- List.range(0, result.length))
                            result(i) = x7
                            internalReshape(x105::Nil, result, "values")
                          }
                          // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(102) and Sym(7))
                          // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(102) and Sym(7))
                          // RuntimeCheck : POST:   V102 = [3  3  3]                                       from Bubble up value for Sym(102) <- InfixOp(-: Sym(102) and Sym(7))
                          // RuntimeCheck : POST:   S102 = [3]                                             from Bubble up shape for Sym(102) <- InfixOp(-: Sym(102) and Sym(7))
                          // RuntimeCheck : PRE:    S102 = S7 OR S7 = []                                   from InfixOp(-: Sym(102) and Sym(7))
                          // Shape: V239=[u181  u180  u179] and S239=[3]
                          val x239: MDArray[Int] = {
                            val result = new Array[Int](shape(x102).content().foldLeft(1)((a,b) => a*b))
                            for(i <- List.range(0, result.length))
                            result(i) = x102.content()(i) -  x7
                            internalReshape(shape(x102), result, "infixOpAA")
                          }
                          // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- InfixOp(-: Sym(237) and Sym(7))
                          // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- InfixOp(-: Sym(237) and Sym(7))
                          // RuntimeCheck : POST:   S237 = [3]                                             from Bubble up shape for Sym(237) <- InfixOp(-: Sym(237) and Sym(7))
                          // RuntimeCheck : PRE:    S237 = S7 OR S7 = []                                   from InfixOp(-: Sym(237) and Sym(7))
                          // Shape: V238=[u399  u398  u397] and S238=[3]
                          val x238: MDArray[Int] = {
                            val result = new Array[Int](shape(x237).content().foldLeft(1)((a,b) => a*b))
                            for(i <- List.range(0, result.length))
                            result(i) = x237.content()(i) -  x7
                            internalReshape(shape(x237), result, "infixOpAA")
                          }
                          // RuntimeCheck : POST:   V242 = [u393]                                          from Bubble up value for Sym(242) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : POST:   S242 = []                                              from Bubble up shape for Sym(242) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : POST:   S240 = [3]                                             from Bubble up shape for Sym(240) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : POST:   V106 = [0  0  0]                                       from Bubble up value for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : POST:   S106 = [3]                                             from Bubble up shape for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : POST:   V108 = [1  1  1]                                       from Bubble up value for Sym(108) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : POST:   S108 = [3]                                             from Bubble up shape for Sym(108) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : POST:   V15 = [u236]                                           from Bubble up value for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : POST:   V239 = [u181  u180  u179]                              from Bubble up value for Sym(239) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : POST:   S239 = [3]                                             from Bubble up shape for Sym(239) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : POST:   V15 = [u236]                                           from Bubble up value for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : POST:   V106 = [0  0  0]                                       from Bubble up value for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : POST:   S106 = [3]                                             from Bubble up shape for Sym(106) <- With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : PRE:    length(S106) = length([u13018])                        from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : PRE:    S239 = S106                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : PRE:    S108 = S106                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : PRE:    S106 = S106                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // RuntimeCheck : PRE:    V106 < V239                                            from With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          // Shape: V243=[u392] and S243=[]
                          // with: With(lb=Sym(106) lbStrict=Sym(15) ubStict=Sym(15) ub=Sym(239) step=Sym(108) width=Sym(106)  Sym(240) => Sym(242))
                          val lb0: Int = x106.content()(0)
                          val ub0: Int = x239.content()(0)
                          val step0: Int = x108.content()(0)
                          val width0: Int = x106.content()(0)
                          val ll0: Int = if (x15) lb0 + 1 else lb0
                          val ul0: Int = if (x15) ub0 else ub0 + 1
                          for (iv0 <- List.range(ll0, ul0)) {
                            if ((iv0 - lb0) % step0 <= width0) {
                              val lb1: Int = x106.content()(1)
                              val ub1: Int = x239.content()(1)
                              val step1: Int = x108.content()(1)
                              val width1: Int = x106.content()(1)
                              val ll1: Int = if (x15) lb1 + 1 else lb1
                              val ul1: Int = if (x15) ub1 else ub1 + 1
                              for (iv1 <- List.range(ll1, ul1)) {
                                if ((iv1 - lb1) % step1 <= width1) {
                                  val lb2: Int = x106.content()(2)
                                  val ub2: Int = x239.content()(2)
                                  val step2: Int = x108.content()(2)
                                  val width2: Int = x106.content()(2)
                                  val ll2: Int = if (x15) lb2 + 1 else lb2
                                  val ul2: Int = if (x15) ub2 else ub2 + 1
                                  for (iv2 <- List.range(ll2, ul2)) {
                                    if ((iv2 - lb2) % step2 <= width2) {
                                      val x240: MDArray[Int] = iv0::iv1::iv2::Nil
                                      val iv: MDArray[Int] = x240
                                      val feval: MDArray[Double] = {
                                        // RuntimeCheck : POST:   V238 = [u399  u398  u397]                              from Bubble up value for Sym(238) <- InfixOp(+: Sym(240) and Sym(238))
                                        // RuntimeCheck : POST:   S238 = [3]                                             from Bubble up shape for Sym(238) <- InfixOp(+: Sym(240) and Sym(238))
                                        // RuntimeCheck : POST:   S240 = [3]                                             from Bubble up shape for Sym(240) <- InfixOp(+: Sym(240) and Sym(238))
                                        // RuntimeCheck : PRE:    S240 = S238 OR S238 = []                               from InfixOp(+: Sym(240) and Sym(238))
                                        // Shape: V241=[u464(<u396)  u465(<u395)  u466(<u394)] and S241=[3]
                                        val x241: MDArray[Int] = {
                                          val result = new Array[Int](shape(x240).content().foldLeft(1)((a,b) => a*b))
                                          for(i <- List.range(0, result.length))
                                          result(i) = x240.content()(i) +  x238.content()(i)
                                          internalReshape(shape(x240), result, "infixOpAA")
                                        }
                                        // RuntimeCheck : POST:   S228 = [u396  u395  u394]                              from Bubble up shape for Sym(228) <- Sel(Sym(241), Sym(228))
                                        // RuntimeCheck : POST:   V241 = [u464(<u396)  u465(<u395)  u466(<u394)]         from Bubble up value for Sym(241) <- Sel(Sym(241), Sym(228))
                                        // RuntimeCheck : POST:   S241 = [3]                                             from Bubble up shape for Sym(241) <- Sel(Sym(241), Sym(228))
                                        // RuntimeCheck : PRE:    length(S241) = length([u13143])                        from Sel(Sym(241), Sym(228))
                                        // RuntimeCheck : PRE:    S228(:length(V241)) < V241                             from Sel(Sym(241), Sym(228))
                                        // Shape: V242=[u393] and S242=[]
                                        
                                        // Shape: V242=[u393] and S242=[]
                                        val x242: Double = x228.content()(flatten(shape(x228), x241, "sel"))
                                        x242
                                      }
                                      // the action of this loop:
                                      if (result == null) {
                                        // create the array and shape
                                        result = new Array[Double](x102.content().foldLeft(1)((a,b) => a*b) * feval.content().length)
                                        rshape = shape(feval).content()
                                      } else {
                                        // check shape -- this WILL be redundant due to runtime checks
                                        if (shape(feval).content().toList != rshape.toList) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                                      }
                                      // copy new content
                                      val mainIndex: Int = flatten(x102 ::: rshape.toList, iv ::: zeros(rshape.length), opName)
                                      for (innerIndex <- List.range(0, rshape.length)) {
                                        result(mainIndex + innerIndex) = feval(innerIndex)
                                      }
                                    } // if ((iv0 ...
                                  } // for (iv0 ...
                                } // if ((iv1 ...
                              } // for (iv1 ...
                            } // if ((iv2 ...
                          } // for (iv2 ...
                          internalReshape(x102 ::: rshape.toList, result, opName)
                        }
                        
                        // RuntimeCheck : POST:   V244 = [u391  u390  u389  u388  u387  u386  u385  u384  u383  u382  u381  u380  u379  u378  u377  u376  u375  u374  u373  u372  u371  u370  u369  u368  u367  u366  u365]     from Bubble up value for Sym(244) <- InfixOp(*: Sym(98) and Sym(244))
                        // RuntimeCheck : POST:   S244 = [3  3  3]                                       from Bubble up shape for Sym(244) <- InfixOp(*: Sym(98) and Sym(244))
                        // RuntimeCheck : POST:   V98 = [u235  u234  u233  u232  u231  u230  u229  u228  u227  u226  u225  u224  u223  u222  u221  u220  u219  u218  u217  u216  u215  u214  u213  u212  u211  u210  u209]     from Bubble up value for Sym(98) <- InfixOp(*: Sym(98) and Sym(244))
                        // RuntimeCheck : POST:   S98 = [3  3  3]                                        from Bubble up shape for Sym(98) <- InfixOp(*: Sym(98) and Sym(244))
                        // RuntimeCheck : PRE:    S98 = S244 OR S244 = []                                from InfixOp(*: Sym(98) and Sym(244))
                        // Shape: V245=[u208  u207  u206  ...  u184  u183  u182] and S245=[3  3  3]
                        val x245: MDArray[Double] = {
                          val result = new Array[Double](shape(x98).content().foldLeft(1)((a,b) => a*b))
                          for(i <- List.range(0, result.length))
                          result(i) = x98.content()(i) *  x244.content()(i)
                          internalReshape(shape(x98), result, "infixOpAA")
                        }
                        // RuntimeCheck : POST:   V245 = [u208  u207  u206  u205  u204  u203  u202  u201  u200  u199  u198  u197  u196  u195  u194  u193  u192  u191  u190  u189  u188  u187  u186  u185  u184  u183  u182]     from Bubble up value for Sym(245) <- Dim(Sym(245))
                        // RuntimeCheck : POST:   S245 = [3  3  3]                                       from Bubble up shape for Sym(245) <- Dim(Sym(245))
                        // Shape: V246=[3] and S246=[]
                        val x246: Int = dim(x245)
                        // RuntimeCheck : POST:   V246 = [3]                                             from Bubble up value for Sym(246) <- FromValue(Sym(246))
                        // RuntimeCheck : POST:   S246 = []                                              from Bubble up shape for Sym(246) <- FromValue(Sym(246))
                        // Shape: V247=[3] and S247=[]
                        val x247: Int = x246
                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(247))
                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(247))
                        // RuntimeCheck : POST:   V247 = [3]                                             from Bubble up value for Sym(247) <- Values(Sym(12), Sym(247))
                        // RuntimeCheck : POST:   S247 = []                                              from Bubble up shape for Sym(247) <- Values(Sym(12), Sym(247))
                        // RuntimeCheck : PRE:    S247 = []                                              from Values(Sym(12), Sym(247))
                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(247))
                        // Shape: V248=[0  0  0] and S248=[3]
                        val x248: MDArray[Int] = {
                          val result = new Array[Int](x247)
                          for(i <- List.range(0, result.length))
                          result(i) = x12
                          internalReshape(x247::Nil, result, "values")
                        }
                        // RuntimeCheck : POST:   V245 = [u208  u207  u206  u205  u204  u203  u202  u201  u200  u199  u198  u197  u196  u195  u194  u193  u192  u191  u190  u189  u188  u187  u186  u185  u184  u183  u182]     from Bubble up value for Sym(245) <- Shape(Sym(245))
                        // RuntimeCheck : POST:   S245 = [3  3  3]                                       from Bubble up shape for Sym(245) <- Shape(Sym(245))
                        // Shape: V249=[3  3  3] and S249=[3]
                        val x249: MDArray[Int] = shape(x245)
                        // RuntimeCheck : POST:   V248 = [0  0  0]                                       from Bubble up value for Sym(248) <- Shape(Sym(248))
                        // RuntimeCheck : POST:   S248 = [3]                                             from Bubble up shape for Sym(248) <- Shape(Sym(248))
                        // Shape: V250=[3] and S250=[1]
                        val x250: MDArray[Int] = shape(x248)
                        // RuntimeCheck : POST:   V250 = [3]                                             from Bubble up value for Sym(250) <- Sel(Sym(9), Sym(250))
                        // RuntimeCheck : POST:   S250 = [1]                                             from Bubble up shape for Sym(250) <- Sel(Sym(9), Sym(250))
                        // RuntimeCheck : POST:   V9 = [0]                                               from Bubble up value for Sym(9) <- Sel(Sym(9), Sym(250))
                        // RuntimeCheck : POST:   S9 = [1]                                               from Bubble up shape for Sym(9) <- Sel(Sym(9), Sym(250))
                        // RuntimeCheck : PRE:    length(S9) = length([u11827])                          from Sel(Sym(9), Sym(250))
                        // RuntimeCheck : PRE:    S250(:length(V9)) < V9                                 from Sel(Sym(9), Sym(250))
                        // Shape: V251=[3] and S251=[]
                        
                        // Shape: V251=[3] and S251=[]
                        val x251: Int = x250.content()(flatten(shape(x250), x9, "sel"))
                        // RuntimeCheck : POST:   V7 = [1]                                               from Bubble up value for Sym(7) <- Values(Sym(7), Sym(251))
                        // RuntimeCheck : POST:   S7 = []                                                from Bubble up shape for Sym(7) <- Values(Sym(7), Sym(251))
                        // RuntimeCheck : POST:   V251 = [3]                                             from Bubble up value for Sym(251) <- Values(Sym(7), Sym(251))
                        // RuntimeCheck : POST:   S251 = []                                              from Bubble up shape for Sym(251) <- Values(Sym(7), Sym(251))
                        // RuntimeCheck : PRE:    S251 = []                                              from Values(Sym(7), Sym(251))
                        // RuntimeCheck : PRE:    S7 = []                                                from Values(Sym(7), Sym(251))
                        // Shape: V252=[1  1  1] and S252=[3]
                        val x252: MDArray[Int] = {
                          val result = new Array[Int](x251)
                          for(i <- List.range(0, result.length))
                          result(i) = x7
                          internalReshape(x251::Nil, result, "values")
                        }
                        // RuntimeCheck : POST:   V12 = [0]                                              from Bubble up value for Sym(12) <- Values(Sym(12), Sym(251))
                        // RuntimeCheck : POST:   S12 = []                                               from Bubble up shape for Sym(12) <- Values(Sym(12), Sym(251))
                        // RuntimeCheck : POST:   V251 = [3]                                             from Bubble up value for Sym(251) <- Values(Sym(12), Sym(251))
                        // RuntimeCheck : POST:   S251 = []                                              from Bubble up shape for Sym(251) <- Values(Sym(12), Sym(251))
                        // RuntimeCheck : PRE:    S251 = []                                              from Values(Sym(12), Sym(251))
                        // RuntimeCheck : PRE:    S12 = []                                               from Values(Sym(12), Sym(251))
                        // Shape: V253=[0  0  0] and S253=[3]
                        val x253: MDArray[Int] = {
                          val result = new Array[Int](x251)
                          for(i <- List.range(0, result.length))
                          result(i) = x12
                          internalReshape(x251::Nil, result, "values")
                        }
                        // RuntimeCheck : POST:   V258 = [u337]                                          from Bubble up value for Sym(258) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   S258 = []                                              from Bubble up shape for Sym(258) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   V257 = [u350(<3)  u351(<3)  u352(<3)]                  from Bubble up value for Sym(257) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   S257 = [3]                                             from Bubble up shape for Sym(257) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   V253 = [0  0  0]                                       from Bubble up value for Sym(253) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   S253 = [3]                                             from Bubble up shape for Sym(253) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   V252 = [1  1  1]                                       from Bubble up value for Sym(252) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   S252 = [3]                                             from Bubble up shape for Sym(252) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   V25 = [u257]                                           from Bubble up value for Sym(25) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   V249 = [3  3  3]                                       from Bubble up value for Sym(249) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   S249 = [3]                                             from Bubble up shape for Sym(249) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   V15 = [u236]                                           from Bubble up value for Sym(15) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   S15 = []                                               from Bubble up shape for Sym(15) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   V248 = [0  0  0]                                       from Bubble up value for Sym(248) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : POST:   S248 = [3]                                             from Bubble up shape for Sym(248) <- With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : PRE:    length(S248) = length([u8345])                         from With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : PRE:    S15 = []                                               from With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : PRE:    S249 = S248                                            from With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : PRE:    S252 = S248                                            from With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : PRE:    S253 = S248                                            from With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // RuntimeCheck : PRE:    V248 < V249                                            from With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        // Shape: V259=[u336] and S259=[]
                        // with: With(lb=Sym(248) lbStrict=Sym(15) ubStict=Sym(25) ub=Sym(249) step=Sym(252) width=Sym(253)  Sym(257) => Sym(258))
                        val lb0: Int = x248.content()(0)
                        val ub0: Int = x249.content()(0)
                        val step0: Int = x252.content()(0)
                        val width0: Int = x253.content()(0)
                        val ll0: Int = if (x15) lb0 + 1 else lb0
                        val ul0: Int = if (x25) ub0 else ub0 + 1
                        for (iv0 <- List.range(ll0, ul0)) {
                          if ((iv0 - lb0) % step0 <= width0) {
                            val lb1: Int = x248.content()(1)
                            val ub1: Int = x249.content()(1)
                            val step1: Int = x252.content()(1)
                            val width1: Int = x253.content()(1)
                            val ll1: Int = if (x15) lb1 + 1 else lb1
                            val ul1: Int = if (x25) ub1 else ub1 + 1
                            for (iv1 <- List.range(ll1, ul1)) {
                              if ((iv1 - lb1) % step1 <= width1) {
                                val lb2: Int = x248.content()(2)
                                val ub2: Int = x249.content()(2)
                                val step2: Int = x252.content()(2)
                                val width2: Int = x253.content()(2)
                                val ll2: Int = if (x15) lb2 + 1 else lb2
                                val ul2: Int = if (x25) ub2 else ub2 + 1
                                for (iv2 <- List.range(ll2, ul2)) {
                                  if ((iv2 - lb2) % step2 <= width2) {
                                    val x257: MDArray[Int] = iv0::iv1::iv2::Nil
                                    val iv: MDArray[Int] = x257
                                    val feval: MDArray[Double] = {
                                      // RuntimeCheck : POST:   V245 = [u208  u207  u206  u205  u204  u203  u202  u201  u200  u199  u198  u197  u196  u195  u194  u193  u192  u191  u190  u189  u188  u187  u186  u185  u184  u183  u182]     from Bubble up value for Sym(245) <- Sel(Sym(257), Sym(245))
                                      // RuntimeCheck : POST:   S245 = [3  3  3]                                       from Bubble up shape for Sym(245) <- Sel(Sym(257), Sym(245))
                                      // RuntimeCheck : POST:   V257 = [u350(<3)  u351(<3)  u352(<3)]                  from Bubble up value for Sym(257) <- Sel(Sym(257), Sym(245))
                                      // RuntimeCheck : POST:   S257 = [3]                                             from Bubble up shape for Sym(257) <- Sel(Sym(257), Sym(245))
                                      // RuntimeCheck : PRE:    length(S257) = length([u12988])                        from Sel(Sym(257), Sym(245))
                                      // RuntimeCheck : PRE:    S245(:length(V257)) < V257                             from Sel(Sym(257), Sym(245))
                                      // Shape: V258=[u337] and S258=[]
                                      
                                      // Shape: V258=[u337] and S258=[]
                                      val x258: Double = x245.content()(flatten(shape(x245), x257, "sel"))
                                      x258
                                    }
                                    // the action of this loop:
                                    result = foldFunction(result, feval)
                                  } // if ((iv0 ...
                                } // for (iv0 ...
                              } // if ((iv1 ...
                            } // for (iv1 ...
                          } // if ((iv2 ...
                        } // for (iv2 ...
                        result
                      }
                      
                      // RuntimeCheck : POST:   V260 = [u240]                                          from Bubble up value for Sym(260) <- ToValue(Sym(260))
                      // RuntimeCheck : POST:   S260 = []                                              from Bubble up shape for Sym(260) <- ToValue(Sym(260))
                      // RuntimeCheck : PRE:    length(S260) = length([])                              from ToValue(Sym(260))
                      // Shape: V261=[u124] and S261=[]
                      val x261: Double = x260
                      // RuntimeCheck : POST:   V261 = [u124]                                          from Bubble up value for Sym(261) <- FromValue(Sym(261))
                      // RuntimeCheck : POST:   S261 = []                                              from Bubble up shape for Sym(261) <- FromValue(Sym(261))
                      // Shape: V262=[u123] and S262=[]
                      val x262: Double = x261
                      x262
                    }
                    // the action of this loop:
                    if (rshape == null) {
                      rshape = shape(x228).drop(iv.content().length)
                    }
                    val mainIndex: Int = flatten(shape(x228), iv ::: zeros(dim(x228) - iv.content().length), opName)
                    // check shape -- this WILL be redundant due to runtime checks
                    if (shape(feval).content().toList != rshape) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                    // copy new content
                    for (innerIndex <- List.range(0, feval.content().length)) {
                      result(mainIndex + innerIndex) = feval.content()(innerIndex)
                    }
                  } // if ((iv0 ...
                } // for (iv0 ...
              } // if ((iv1 ...
            } // for (iv1 ...
          } // if ((iv2 ...
        } // for (iv2 ...
        internalReshape(shape(x228) ::: rshape.toList, result, opName)
      }
      
      // RuntimeCheck : POST:   S264 = [u396  u395  u394]                              from Bubble up shape for Sym(264) <- InfixOp(+: Sym(230) and Sym(264))
      // RuntimeCheck : POST:   S230 = [u396  u395  u394]                              from Bubble up shape for Sym(230) <- InfixOp(+: Sym(230) and Sym(264))
      // RuntimeCheck : PRE:    S230 = S264 OR S264 = []                               from InfixOp(+: Sym(230) and Sym(264))
      // Shape: S265=[u396  u395  u394]
      val x265: MDArray[Double] = {
        val result = new Array[Double](shape(x230).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x230.content()(i) +  x264.content()(i)
        internalReshape(shape(x230), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   V49 = [u11]                                            from Bubble up value for Sym(49) <- InfixOp(*: Sym(265) and Sym(49))
      // RuntimeCheck : POST:   S49 = []                                               from Bubble up shape for Sym(49) <- InfixOp(*: Sym(265) and Sym(49))
      // RuntimeCheck : POST:   S265 = [u396  u395  u394]                              from Bubble up shape for Sym(265) <- InfixOp(*: Sym(265) and Sym(49))
      // RuntimeCheck : PRE:    S265 = S49 OR S49 = []                                 from InfixOp(*: Sym(265) and Sym(49))
      // Shape: S266=[u396  u395  u394]
      val x266: MDArray[Double] = {
        val result = new Array[Double](shape(x265).content().foldLeft(1)((a,b) => a*b))
        for(i <- List.range(0, result.length))
        result(i) = x265.content()(i) *  x49
        internalReshape(shape(x265), result, "infixOpAA")
      }
      // RuntimeCheck : POST:   V269 = [u451]                                          from Bubble up value for Sym(269) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   S269 = []                                              from Bubble up shape for Sym(269) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   V268 = [u458(<u396)  u459(<u395)  u460(<u394)]         from Bubble up value for Sym(268) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   S268 = [3]                                             from Bubble up shape for Sym(268) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   V234 = [0  0  0]                                       from Bubble up value for Sym(234) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   S234 = [3]                                             from Bubble up shape for Sym(234) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   V236 = [1  1  1]                                       from Bubble up value for Sym(236) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   S236 = [3]                                             from Bubble up shape for Sym(236) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   V25 = [u257]                                           from Bubble up value for Sym(25) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   V267 = [u405  u404  u403]                              from Bubble up value for Sym(267) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   S267 = [3]                                             from Bubble up shape for Sym(267) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   V25 = [u257]                                           from Bubble up value for Sym(25) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   S25 = []                                               from Bubble up shape for Sym(25) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   V234 = [0  0  0]                                       from Bubble up value for Sym(234) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : POST:   S234 = [3]                                             from Bubble up shape for Sym(234) <- With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : PRE:    length(S234) = length([u524])                          from With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : PRE:    S25 = []                                               from With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : PRE:    S267 = S234                                            from With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : PRE:    S236 = S234                                            from With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : PRE:    S234 = S234                                            from With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // RuntimeCheck : PRE:    V234 < V267                                            from With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      // Shape: V270=[u450] and S270=[]
      // with: With(lb=Sym(234) lbStrict=Sym(25) ubStict=Sym(25) ub=Sym(267) step=Sym(236) width=Sym(234)  Sym(268) => Sym(269))
      val lb0: Int = x234.content()(0)
      val ub0: Int = x267.content()(0)
      val step0: Int = x236.content()(0)
      val width0: Int = x234.content()(0)
      val ll0: Int = if (x25) lb0 + 1 else lb0
      val ul0: Int = if (x25) ub0 else ub0 + 1
      for (iv0 <- List.range(ll0, ul0)) {
        if ((iv0 - lb0) % step0 <= width0) {
          val lb1: Int = x234.content()(1)
          val ub1: Int = x267.content()(1)
          val step1: Int = x236.content()(1)
          val width1: Int = x234.content()(1)
          val ll1: Int = if (x25) lb1 + 1 else lb1
          val ul1: Int = if (x25) ub1 else ub1 + 1
          for (iv1 <- List.range(ll1, ul1)) {
            if ((iv1 - lb1) % step1 <= width1) {
              val lb2: Int = x234.content()(2)
              val ub2: Int = x267.content()(2)
              val step2: Int = x236.content()(2)
              val width2: Int = x234.content()(2)
              val ll2: Int = if (x25) lb2 + 1 else lb2
              val ul2: Int = if (x25) ub2 else ub2 + 1
              for (iv2 <- List.range(ll2, ul2)) {
                if ((iv2 - lb2) % step2 <= width2) {
                  val x268: MDArray[Int] = iv0::iv1::iv2::Nil
                  val iv: MDArray[Int] = x268
                  val feval: MDArray[Double] = {
                    // RuntimeCheck : POST:   S266 = [u396  u395  u394]                              from Bubble up shape for Sym(266) <- Sel(Sym(268), Sym(266))
                    // RuntimeCheck : POST:   V268 = [u458(<u396)  u459(<u395)  u460(<u394)]         from Bubble up value for Sym(268) <- Sel(Sym(268), Sym(266))
                    // RuntimeCheck : POST:   S268 = [3]                                             from Bubble up shape for Sym(268) <- Sel(Sym(268), Sym(266))
                    // RuntimeCheck : PRE:    length(S268) = length([u4433])                         from Sel(Sym(268), Sym(266))
                    // RuntimeCheck : PRE:    S266(:length(V268)) < V268                             from Sel(Sym(268), Sym(266))
                    // Shape: V269=[u451] and S269=[]
                    
                    // Shape: V269=[u451] and S269=[]
                    val x269: Double = x266.content()(flatten(shape(x266), x268, "sel"))
                    x269
                  }
                  // the action of this loop:
                  if (rshape == null) {
                    rshape = shape(x228).drop(iv.content().length)
                  }
                  val mainIndex: Int = flatten(shape(x228), iv ::: zeros(dim(x228) - iv.content().length), opName)
                  // check shape -- this WILL be redundant due to runtime checks
                  if (shape(feval).content().toList != rshape) throw new Exception(opName + ": Incompatible shapes:" + rshape.toList.toString + " vs " + shape(feval).content().toList.toString)
                  // copy new content
                  for (innerIndex <- List.range(0, feval.content().length)) {
                    result(mainIndex + innerIndex) = feval.content()(innerIndex)
                  }
                } // if ((iv0 ...
              } // for (iv0 ...
            } // if ((iv1 ...
          } // for (iv1 ...
        } // if ((iv2 ...
      } // for (iv2 ...
      internalReshape(shape(x228) ::: rshape.toList, result, opName)
    }
    
    // RuntimeCheck : POST:   S228 = [u396  u395  u394]                              from Bubble up shape for Sym(228) <- Where(Sym(229), Sym(271), Sym(228))
    // RuntimeCheck : POST:   S271 = [u396  u395  u394]                              from Bubble up shape for Sym(271) <- Where(Sym(229), Sym(271), Sym(228))
    // RuntimeCheck : POST:   S229 = [u396  u395  u394]                              from Bubble up shape for Sym(229) <- Where(Sym(229), Sym(271), Sym(228))
    // RuntimeCheck : PRE:    S229 = S271                                            from Where(Sym(229), Sym(271), Sym(228))
    // RuntimeCheck : PRE:    S229 = S228                                            from Where(Sym(229), Sym(271), Sym(228))
    // Shape: S272=[u396  u395  u394]
    val x272: MDArray[Double] = {
      val result = new Array[Double](shape(x271).content().foldLeft(1)((a,b) => a*b))
      for(i <- List.range(0, result.length))
      result(i) = if (x229.content()(i)) x271.content()(i) else x228.content()(i)
      internalReshape(shape(x271), result, "where")
    }
    x272
  }
}
/*****************************************
  End of Generated Code                  
*******************************************/
