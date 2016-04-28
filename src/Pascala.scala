import scala.collection.mutable
import scala.language.dynamics
import scala.util.control.Breaks

class Pascala extends App {
  abstract sealed class Sentence
  case class ProgramSentence(streams: String) extends Sentence
  case class BeginSentence() extends Sentence
  case class EndSentence() extends Sentence
  case class WriteSentence(value: Any) extends Sentence
  case class DeclareSentence(s: Symbol) extends Sentence
  case class ReadSentence(s: Symbol) extends Sentence
  case class WhileSentence(cond: ExprSentence) extends Sentence
  case class DoSentence() extends Sentence
  case class IfSentence(cond: ExprSentence) extends Sentence
  case class ThenSentence() extends Sentence
  case class ElseSentence() extends Sentence
  case class ExprSentence(op: String, lhs: Any, rhs: Any) extends Sentence
  case class AssignmentSentence(sym: Symbol, value: Any) extends Sentence

  var program = new mutable.ArrayBuffer[Sentence]

  var doubles = new mutable.HashMap[Symbol, Double]()
  var ints = new mutable.HashMap[Symbol, Int]()
  var bools = new mutable.HashMap[Symbol, Boolean]()
  var strings = new mutable.HashMap[Symbol, String]()

  // for checking if a variable is declared before being assigned
  var declared = new mutable.HashMap[Symbol, Boolean]()

  def RUN() = execute(program)

  def Begin = program.append(BeginSentence())

  def Var(fn: () => Symbol) = program.append(DeclareSentence(fn()))

  object If {
    def apply(cond: ExprSentence) = program.append(IfSentence(cond))
  }

  object While {
    def apply(cond: ExprSentence) = program.append(WhileSentence(cond))
  }

  def Do = program.append(DoSentence())
  def Then = program.append(ThenSentence())
  def Else = program.append(ElseSentence())

  case class Symbol2Assignment(sym: Symbol) {
    def :=(value:Any): Unit = program.append(AssignmentSentence(sym,value))
  }
  def assignment(sym: Symbol, value: Any): Unit ={
    if(value.isInstanceOf[ExprSentence]) {
      val tmp = evalExpr(value.asInstanceOf[ExprSentence])()
      if (tmp.isInstanceOf[Int]) {
        ints(sym) = tmp.asInstanceOf[Int]
      } else if (tmp.isInstanceOf[Double]) {
        doubles(sym) = tmp.asInstanceOf[Double]
      } else if (tmp.isInstanceOf[String]) {
        strings(sym) = tmp.asInstanceOf[String]
      } else if (tmp.isInstanceOf[Boolean]) {
        bools(sym) = tmp.asInstanceOf[Boolean]
      } else {
        throw new IllegalStateException("Must assign int, boolean, double, or String value")
      }
    } else if(value.isInstanceOf[Int]){
      ints(sym) = value.asInstanceOf[Int]
    } else if(value.isInstanceOf[Double]){
      doubles(sym) = value.asInstanceOf[Double]
    } else if(value.isInstanceOf[String]){
      strings(sym) = value.asInstanceOf[String]
    } else if(value.isInstanceOf[Boolean]){
      bools(sym) = value.asInstanceOf[Boolean]
    } else {
      throw new IllegalStateException("Must assign int, boolean, double, or String value")
    }
  }

  case class EvalSymbol(lhs: Symbol) {
    def +(rhs: Int): Function0[Int] = () => ints(lhs) + rhs
    def +(rhs: Function0[Any]): Function0[Any] = {
      if (rhs().isInstanceOf[Symbol]) {
        if (ints.contains(lhs)) {
          () => ints(lhs) + ints(rhs().asInstanceOf[Symbol])
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) + doubles(rhs().asInstanceOf[Symbol])
        } else if (strings.contains(lhs)) {
          () => strings(lhs) + strings(rhs().asInstanceOf[Symbol])
        } else {
          throw new IllegalStateException("LHS must be a int, string, or double")
        }
      } else {
        if (ints.contains(lhs)) {
          () => ints(lhs) + rhs().asInstanceOf[Int]
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) + rhs().asInstanceOf[Double]
        } else if (strings.contains(lhs)) {
          () => strings(lhs) + rhs().asInstanceOf[String]
        } else {
          println(lhs)
          throw new IllegalStateException("LHS must be a int, string, or double")
        }
      }
    }
    def +(rhs: Double): Function0[Double] = () => doubles(lhs) + rhs
    def +(rhs: String): Function0[String] = () => strings(lhs) + rhs
    def +(rhs: Symbol): Function0[Any] = {
      if (ints.contains(rhs)) {
        () => ints(lhs) + ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => doubles(lhs) + doubles(rhs)
      } else if (strings.contains(rhs)) {
        () => strings(lhs) + strings(rhs)
      } else {
        throw new IllegalStateException("Cannot use plus operator with booleans.")
      }
    }

    def -(rhs: Int): Function0[Int] = () => ints(lhs) - rhs
    def -(rhs: Double): Function0[Double] = () => doubles(lhs) - rhs
    def -(rhs: Function0[Any]): Function0[Any] = {
      if (rhs().isInstanceOf[Symbol]) {
        if (ints.contains(lhs)) {
          () => ints(lhs) - ints(rhs().asInstanceOf[Symbol])
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) - doubles(rhs().asInstanceOf[Symbol])
        } else {
          throw new IllegalStateException("LHS must be a int or double")
        }
      } else {
        if (ints.contains(lhs)) {
          () => ints(lhs) - rhs().asInstanceOf[Int]
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) - rhs().asInstanceOf[Double]
        } else {
          println(lhs)
          throw new IllegalStateException("LHS must be a int or double")
        }
      }
    }
    def -(rhs: Symbol): Function0[Any] = {
      if (ints.contains(rhs)) {
        () => ints(lhs) - ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => doubles(lhs) - doubles(rhs)
      } else {
        throw new IllegalStateException("Cannot use minus operator with strings or booleans.")
      }
    }

    def *(rhs: Int): Function0[Int] = () => ints(lhs) * rhs
    def *(rhs: Double): Function0[Double] = () => doubles(lhs) * rhs
    def *(rhs: Function0[Any]): Function0[Any] = {
      if (rhs().isInstanceOf[Symbol]) {
        if (ints.contains(lhs)) {
          () => ints(lhs) * ints(rhs().asInstanceOf[Symbol])
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) * doubles(rhs().asInstanceOf[Symbol])
        } else {
          throw new IllegalStateException("Cannot use mult operator with strings or booleans")
        }
      } else {
        if (ints.contains(lhs)) {
          () => ints(lhs) * rhs().asInstanceOf[Int]
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) * rhs().asInstanceOf[Double]
        } else {
          println(lhs)
          throw new IllegalStateException("Cannot use mult operator with strings or booleans")
        }
      }
    }
    def *(rhs: Symbol): Function0[Any] = {
      if (ints.contains(rhs)) {
        () => ints(lhs) * ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => doubles(lhs) * doubles(rhs)
      } else {
        throw new IllegalStateException("Cannot use multiplication operator with strings or booleans.")
      }
    }

    def div(rhs: Int): Function0[Int] = () => ints(lhs) / rhs
    def div(rhs: Double): Function0[Double] = () => doubles(lhs) / rhs
    def div(rhs: Function0[Any]): Function0[Any] = {
      if (rhs().isInstanceOf[Symbol]) {
        if (ints.contains(lhs)) {
          () => ints(lhs) / ints(rhs().asInstanceOf[Symbol])
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) / doubles(rhs().asInstanceOf[Symbol])
        } else {
          throw new IllegalStateException("Cannot use div operator with strings or booleans")
        }
      } else {
        if (ints.contains(lhs)) {
          () => ints(lhs) / rhs().asInstanceOf[Int]
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) / rhs().asInstanceOf[Double]
        } else {
          println(lhs)
          throw new IllegalStateException("Cannot use div operator with strings or booleans")
        }
      }
    }
    def div(rhs: Symbol): Function0[Any] = {
      if (ints.contains(rhs)) {
        () => ints(lhs) / ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => doubles(lhs) / doubles(rhs)
      } else {
        throw new IllegalStateException("Cannot use multiplication operator with strings or booleans.")
      }
    }

    def >(rhs: Int): Function0[Boolean] = () => ints(lhs) > rhs
    def >(rhs: Double): Function0[Boolean] = () => doubles(lhs) > rhs
    def >(rhs: Symbol): Function0[Boolean] = {
      if (ints.contains(rhs)) {
        () => ints(lhs) > ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => doubles(lhs) > doubles(rhs)
      } else {
        throw new IllegalStateException("Cannot use the > operator with strings or booleans.")
      }
    }
    def >(rhs: Function0[Any]): Function0[Any] = {
      if (rhs().isInstanceOf[Symbol]) {
        if (ints.contains(lhs)) {
          () => ints(lhs) > ints(rhs().asInstanceOf[Symbol])
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) > doubles(rhs().asInstanceOf[Symbol])
        } else {
          println(lhs)
          throw new IllegalStateException("Cannot use > operator with strings or booleans.")
        }
      } else {
        if (ints.contains(lhs)) {
          () => ints(lhs) > rhs().asInstanceOf[Int]
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) > rhs().asInstanceOf[Double]
        } else {
          println(lhs)
          throw new IllegalStateException("Cannot use > operator with strings or booleans.")
        }
      }
    }

    def >=(rhs: Int): Function0[Boolean] = () => ints(lhs) >= rhs
    def >=(rhs: Double): Function0[Boolean] = () => doubles(lhs) >= rhs
    def >=(rhs: Symbol): Function0[Boolean] = {
      if (ints.contains(rhs)) {
        () => ints(lhs) >= ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => doubles(lhs) >= doubles(rhs)
      } else {
        throw new IllegalStateException("Cannot use the > operator with strings or booleans.")
      }
    }
    def >=(rhs: Function0[Any]): Function0[Any] = {
      if (rhs().isInstanceOf[Symbol]) {
        if (ints.contains(lhs)) {
          () => ints(lhs) >= ints(rhs().asInstanceOf[Symbol])
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) >= doubles(rhs().asInstanceOf[Symbol])
        } else {
          println(lhs)
          throw new IllegalStateException("Cannot use >= operator with strings or booleans.")
        }
      } else {
        if (ints.contains(lhs)) {
          () => ints(lhs) >= rhs().asInstanceOf[Int]
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) >= rhs().asInstanceOf[Double]
        } else {
          println(lhs)
          throw new IllegalStateException("Cannot use >= operator with strings or booleans.")
        }
      }
    }

    def ==(rhs: Int): Function0[Boolean] = () => ints(lhs) == rhs
    def ==(rhs: Double): Function0[Boolean] = () => doubles(lhs) == rhs
    def ==(rhs: String): Function0[Boolean] = () => strings(lhs).equals(rhs)
    def ==(rhs: Boolean): Function0[Boolean] = () => bools(lhs) == rhs
    def ==(rhs: Symbol): Function0[Boolean] = {
      if (ints.contains(rhs)) {
        () => ints(lhs) == ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => doubles(lhs) == doubles(rhs)
      } else if (strings.contains(rhs)) {
        () => strings(lhs).equals(strings(rhs))
      } else {
        throw new IllegalStateException("Cannot use the == operator with strings or booleans.")
      }
    }
    def ==(rhs: Function0[Any]): Function0[Any] = {
      if (rhs().isInstanceOf[Symbol]) {
        if (ints.contains(lhs)) {
          () => ints(lhs) == ints(rhs().asInstanceOf[Symbol])
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) == doubles(rhs().asInstanceOf[Symbol])
        } else if (strings.contains(lhs)) {
          () => strings(lhs) == strings(rhs().asInstanceOf[Symbol])
        } else {
          () => bools(lhs) == bools(rhs().asInstanceOf[Symbol])
        }
      } else {
        if (ints.contains(lhs)) {
          () => ints(lhs) == rhs().asInstanceOf[Int]
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) == rhs().asInstanceOf[Double]
        } else if (strings.contains(lhs)) {
          () => strings(lhs) == rhs().asInstanceOf[String]
        } else {
          () => bools(lhs) == rhs().asInstanceOf[Boolean]
        }
      }
    }

    def <=(rhs: Int): Function0[Boolean] = () => ints(lhs) <= rhs
    def <=(rhs: Double): Function0[Boolean] = () => doubles(lhs) <= rhs
    def <=(rhs: Symbol): Function0[Boolean] = {
      if (ints.contains(rhs)) {
        () => ints(lhs) <= ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => doubles(lhs) <= doubles(rhs)
      } else {
        throw new IllegalStateException("Cannot use the <= operator with strings or booleans.")
      }
    }
    def <=(rhs: Function0[Any]): Function0[Any] = {
      if (rhs().isInstanceOf[Symbol]) {
        if (ints.contains(lhs)) {
          () => ints(lhs) <= ints(rhs().asInstanceOf[Symbol])
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) <= doubles(rhs().asInstanceOf[Symbol])
        } else {
          println(lhs)
          throw new IllegalStateException("Cannot use <= operator with strings or booleans.")
        }
      } else {
        if (ints.contains(lhs)) {
          () => ints(lhs) <= rhs().asInstanceOf[Int]
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) <= rhs().asInstanceOf[Double]
        } else {
          println(lhs)
          throw new IllegalStateException("Cannot use <= operator with strings or booleans.")
        }
      }
    }

    def <(rhs: Int): Function0[Boolean] = () => ints(lhs) < rhs
    def <(rhs: Double): Function0[Boolean] = () => doubles(lhs) < rhs
    def <(rhs: Symbol): Function0[Boolean] = {
      if (ints.contains(rhs)) {
        () => ints(lhs) < ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => doubles(lhs) < doubles(rhs)
      } else {
        throw new IllegalStateException("Cannot use the < operator with strings or booleans.")
      }
    }
    def <(rhs: Function0[Any]): Function0[Any] = {
      if (rhs().isInstanceOf[Symbol]) {
        if (ints.contains(lhs)) {
          () => ints(lhs) < ints(rhs().asInstanceOf[Symbol])
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) < doubles(rhs().asInstanceOf[Symbol])
        } else {
          println(lhs)
          throw new IllegalStateException("Cannot use < operator with strings or booleans.")
        }
      } else {
        if (ints.contains(lhs)) {
          () => ints(lhs) < rhs().asInstanceOf[Int]
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) < rhs().asInstanceOf[Double]
        } else {
          println(lhs)
          throw new IllegalStateException("Cannot use < operator with strings or booleans.")
        }
      }
    }


    def <>(rhs: Int): Function0[Boolean] = () => ints(lhs) != rhs
    def <>(rhs: Double): Function0[Boolean] = () => doubles(lhs) != rhs
    def <>(rhs: String): Function0[Boolean] = () => strings(lhs).equals(rhs)
    def <>(rhs: Boolean): Function0[Boolean] = () => bools(lhs) != rhs
    def <>(rhs: Symbol): Function0[Boolean] = {
      if (ints.contains(rhs)) {
        () => ints(lhs) != ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => doubles(lhs) != doubles(rhs)
      } else if (strings.contains(rhs)) {
        () => strings(lhs).equals(strings(rhs))
      } else {
        throw new IllegalStateException("Cannot use the != operator with strings or booleans.")
      }
    }
    def <>(rhs: Function0[Any]): Function0[Any] = {
      if (rhs().isInstanceOf[Symbol]) {
        if (ints.contains(lhs)) {
          () => ints(lhs) != ints(rhs().asInstanceOf[Symbol])
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) != doubles(rhs().asInstanceOf[Symbol])
        } else if (strings.contains(lhs)) {
          () => strings(lhs) != strings(rhs().asInstanceOf[Symbol])
        } else {
          () => bools(lhs) != bools(rhs().asInstanceOf[Symbol])
        }
      } else {
        if (ints.contains(lhs)) {
          () => ints(lhs) != rhs().asInstanceOf[Int]
        } else if (doubles.contains(lhs)) {
          () => doubles(lhs) != rhs().asInstanceOf[Double]
        } else if (strings.contains(lhs)) {
          () => strings(lhs) != rhs().asInstanceOf[String]
        } else {
          () => bools(lhs) != rhs().asInstanceOf[Boolean]
        }
      }
    }
  }

  case class EvalAny(lhs: Any) {
    def +(rhs: Int): Function0[Int] = () => lhs.asInstanceOf[Int] + rhs
    def +(rhs: Function0[Any]): Function0[Any] = {
      if (rhs().isInstanceOf[Symbol]) {
        if (lhs.isInstanceOf[Int]) {
          () => lhs.asInstanceOf[Int] + ints(rhs().asInstanceOf[Symbol])
        } else if (lhs.isInstanceOf[Double]) {
          () => lhs.asInstanceOf[Double] + doubles(rhs().asInstanceOf[Symbol])
        } else if (lhs.isInstanceOf[String]) {
          () =>lhs.asInstanceOf[String] + strings(rhs().asInstanceOf[Symbol])
        } else {
          throw new IllegalStateException("LHS must be a int, string, or double")
        }
      } else {
        if (lhs.isInstanceOf[Int]) {
          () => lhs.asInstanceOf[Int] + rhs().asInstanceOf[Int]
        } else if (lhs.isInstanceOf[Double]) {
          () => lhs.asInstanceOf[Double] + rhs().asInstanceOf[Double]
        } else if (lhs.isInstanceOf[String]) {
          () => lhs.asInstanceOf[String] + rhs().asInstanceOf[String]
        } else {
          throw new IllegalStateException("LHS must be a int, string, or double")
        }
      }
    }
    def +(rhs: Double): Function0[Double] = () => lhs.asInstanceOf[Double] + rhs
    def +(rhs: String): Function0[String] = () => lhs.asInstanceOf[String] + rhs
    def +(rhs: Symbol): Function0[Any] = {
      if (ints.contains(rhs)) {
        () => lhs.asInstanceOf[Int] + ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => lhs.asInstanceOf[Double] + doubles(rhs)
      } else if (strings.contains(rhs)) {
        () => lhs.asInstanceOf[String] + strings(rhs)
      } else {
        throw new IllegalStateException("Cannot use plus operator with booleans.")
      }
    }
  }

  case class EvalFunction0(lhs: Function0[Any]) {
    def +(rhs: Int): Function0[Int] = () => lhs().asInstanceOf[Int] + rhs
    def +(rhs: Double): Function0[Double] = () => lhs().asInstanceOf[Double] + rhs
    def +(rhs: String): Function0[String] = () => lhs().asInstanceOf[String] + rhs
    def +(rhs: Symbol): Function0[Any] = {
      if (ints.contains(rhs)) {
        () => lhs().asInstanceOf[Int] + ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => lhs().asInstanceOf[Double] + doubles(rhs)
      } else if (strings.contains(rhs)) {
        () => lhs().asInstanceOf[String] + strings(rhs)
      } else {
        throw new IllegalStateException("Cannot use plus operator with booleans.")
      }
    }
    def +(rhs: Function0[Any]): Function0[Any] = {
      if (rhs().isInstanceOf[Symbol]) {
        if (lhs().isInstanceOf[Int]) {
          () => lhs().asInstanceOf[Int] + ints(rhs().asInstanceOf[Symbol])
        } else if (lhs().isInstanceOf[Double]) {
          () => lhs().asInstanceOf[Double] + doubles(rhs().asInstanceOf[Symbol])
        } else if (lhs().isInstanceOf[String]) {
          () => lhs().asInstanceOf[String] + strings(rhs().asInstanceOf[Symbol])
        } else {
          throw new IllegalStateException("LHS must be a int, string, or double")
        }
      } else {
        lhs() match {
          case i: Int =>
            () => i + rhs().asInstanceOf[Int]
          case d: Double =>
            () => d + rhs().asInstanceOf[Double]
          case s: String =>
            () => s + rhs().asInstanceOf[String]
          case sym: Symbol =>
            if (ints.contains(sym)) {
              () => lhs().asInstanceOf[Int] + ints(sym)
            } else if (doubles.contains(sym)) {
              () => lhs().asInstanceOf[Double] + doubles(sym)
            } else if (strings.contains(sym)) {
              () => lhs().asInstanceOf[String] + strings(sym)
            } else {
              throw new IllegalStateException("Cannot use plus operator with booleans.")
            }
          case _ =>
            throw new IllegalStateException("Cannot use the addition operator with booleans.")
        }
      }
    }

    def -(rhs: Int): Function0[Int] = () => lhs().asInstanceOf[Int] - rhs
    def -(rhs: Double): Function0[Double] = () => lhs().asInstanceOf[Double] - rhs
    def -(rhs: Symbol): Function0[Any] = {
      if (ints.contains(rhs)) {
        () => lhs().asInstanceOf[Int] - ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => lhs().asInstanceOf[Double] - doubles(rhs)
      } else {
        throw new IllegalStateException("Cannot use minus operator with booleans or strings.")
      }
    }
    def -(rhs: Function0[Any]): Function0[Any] = {
      lhs() match {
        case i: Int =>
          () => i - rhs().asInstanceOf[Int]
        case d: Double =>
          () => d - rhs().asInstanceOf[Double]
        case _ =>
          throw new IllegalStateException("Cannot use the minus operator without strings or booleans.")
      }
    }

    def *(rhs: Int): Function0[Int] = () => lhs().asInstanceOf[Int] * rhs
    def *(rhs: Double): Function0[Double] = () => lhs().asInstanceOf[Double] * rhs
    def *(rhs: Symbol): Function0[Any] = {
      if (ints.contains(rhs)) {
        () => lhs().asInstanceOf[Int] * ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => lhs().asInstanceOf[Double] * doubles(rhs)
      } else {
        throw new IllegalStateException("Cannot use multiplication operator with booleans or strings.")
      }
    }
    def *(rhs: Function0[Any]): Function0[Any] = {
      lhs() match {
        case i: Int =>
          () => i * rhs().asInstanceOf[Int]
        case d: Double =>
          () => d * rhs().asInstanceOf[Double]
        case _ =>
          throw new IllegalStateException("Cannot use the multiplication operator without ints or doubles.")
      }
    }

    def div(rhs: Int): Function0[Int] = () => lhs().asInstanceOf[Int] / rhs
    def div(rhs: Double): Function0[Double] = () => lhs().asInstanceOf[Double] / rhs
    def div(rhs: Symbol): Function0[Any] = {
      if (ints.contains(rhs)) {
        () => lhs().asInstanceOf[Int] / ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => lhs().asInstanceOf[Double] / doubles(rhs)
      } else {
        throw new IllegalStateException("Cannot use division operator with booleans or strings.")
      }
    }
    def div(rhs: Function0[Any]): Function0[Any] = {
      lhs() match {
        case i: Int =>
          () => i / rhs().asInstanceOf[Int]
        case d: Double =>
          () => d / rhs().asInstanceOf[Double]
        case _ =>
          throw new IllegalStateException("Cannot use the division operator without ints or doubles.")
      }
    }
  }

  object Program {
    def apply(streams: String) = program.append(ProgramSentence(streams))
  }

  object Readln {
    def apply(sym: Symbol) = program.append(ReadSentence(sym))
  }

  object Writeln {
    def apply(s: String) = program.append(WriteSentence(s))
    def apply(i: Int) = program.append(WriteSentence(i.toString))
    def apply(d: Double) = program.append(WriteSentence(d.toString))
    def apply(b: Boolean) = program.append(WriteSentence(b.toString))
    def apply(fn: Function0[Any]) = program.append(WriteSentence(fn().toString))
    def apply(sym: Symbol) = program.append(WriteSentence(sym))
    def apply(expr: ExprSentence) = program.append(WriteSentence(expr))
  }

  def getSymVal(sym: Symbol) = {
    if (ints.contains(sym)) {
      ints(sym)
    } else if (doubles.contains(sym)) {
      doubles(sym)
    } else if (strings.contains(sym)) {
      strings(sym)
    } else {
      bools(sym)
    }
  }

  case class Symbol2Expr(lhs: Symbol) {
    def +(rhs: Any): ExprSentence = ExprSentence("+", lhs, rhs)
    def -(rhs: Any): ExprSentence = ExprSentence("-", lhs, rhs)
    def *(rhs: Any): ExprSentence = ExprSentence("*", lhs, rhs)
    def div(rhs: Any): ExprSentence = ExprSentence("div", lhs, rhs)
    def <(rhs: Any): ExprSentence = ExprSentence("<", lhs, rhs)
    def >(rhs: Any): ExprSentence = ExprSentence(">", lhs, rhs)
    def >=(rhs: Any): ExprSentence = ExprSentence(">=", lhs, rhs)
    def <=(rhs: Any): ExprSentence = ExprSentence("<=", lhs, rhs)
    def ===(rhs: Any): ExprSentence = ExprSentence("==", lhs, rhs)
    def <>(rhs: Any): ExprSentence = ExprSentence("<>", lhs, rhs)
  }

  case class Expr2Expr(lhs: ExprSentence) {
    def +(rhs: Any): ExprSentence = ExprSentence("+", lhs, rhs)
    def -(rhs: Any): ExprSentence = ExprSentence("-", lhs, rhs)
    def *(rhs: Any): ExprSentence = ExprSentence("*", lhs, rhs)
    def div(rhs: Any): ExprSentence = ExprSentence("div", lhs, rhs)
  }

  case class Int2Expr(lhs: Int) {
    def +(rhs: Any): ExprSentence = ExprSentence("+", lhs, rhs)
    def -(rhs: Any): ExprSentence = ExprSentence("-", lhs, rhs)
    def *(rhs: Any): ExprSentence = ExprSentence("*", lhs, rhs)
    def div(rhs: Any): ExprSentence = ExprSentence("div", lhs, rhs)
  }

  case class Double2Expr(lhs: Double) {
    def +(rhs: Any): ExprSentence = ExprSentence("+", lhs, rhs)
    def -(rhs: Any): ExprSentence = ExprSentence("-", lhs, rhs)
    def *(rhs: Any): ExprSentence = ExprSentence("*", lhs, rhs)
    def div(rhs: Any): ExprSentence = ExprSentence("div", lhs, rhs)
  }

  case class String2Expr(lhs: String) {
    def +(rhs: Any): ExprSentence = ExprSentence("+", lhs, rhs)
  }

  def End = program.append(EndSentence())

  def evalExpr(expr: ExprSentence): Function0[Any] = {
    expr.op match {
      case "+" => {
        expr.lhs match {
          case sym:Symbol => EvalSymbol(sym).+(() => expr.rhs)
          case e:ExprSentence => EvalFunction0(evalExpr(e)).+(() => expr.rhs)
          case a:Any => EvalAny(a).+(() => expr.rhs)
        }
      }
      case "-" => {
        expr.lhs match {
          case sym:Symbol => EvalSymbol(sym).-(() => expr.rhs)
          case e:ExprSentence => EvalFunction0(evalExpr(e)).-(() => expr.rhs)
          //case a:Any => EvalAny(a).-((() => expr.rhs))
        }
      }
      case "*" => {
        expr.lhs match {
          case sym:Symbol => EvalSymbol(sym).*(() => expr.rhs)
          case e:ExprSentence => EvalFunction0(evalExpr(e)).*(() => expr.rhs)
          //case a:Any => EvalAny(a).-((() => expr.rhs))
        }
      }
      case "div" => {
        expr.lhs match {
          case sym:Symbol => EvalSymbol(sym).div(() => expr.rhs)
          case e:ExprSentence => EvalFunction0(evalExpr(e)).div(() => expr.rhs)
          //case a:Any => EvalAny(a).-((() => expr.rhs))
        }
      }
      case "<" => {
        expr.lhs match{
          case sym:Symbol => EvalSymbol(sym).<(() => expr.rhs)
        }
      }
      case ">" => {
        expr.lhs match{
          case sym:Symbol => EvalSymbol(sym).>(() => expr.rhs)
        }
      }
      case ">=" => {
        expr.lhs match{
          case sym:Symbol => EvalSymbol(sym).>=(() => expr.rhs)
        }
      }
      case "<=" => {
        expr.lhs match{
          case sym:Symbol => EvalSymbol(sym).<=(() => expr.rhs)
        }
      }
      case "==" => {
        expr.lhs match{
          case sym:Symbol => EvalSymbol(sym).==(() => expr.rhs)
        }
      }
      case "<>" => {
        expr.lhs match{
          case sym:Symbol => EvalSymbol(sym).<>(() => expr.rhs)
        }
      }
    }
  }

  def execute(lines: mutable.ArrayBuffer[Sentence]): Unit = {
    val head: Sentence = lines.head
    head match {
      case ProgramSentence(name: String) => {
        execute(lines.slice(1, lines.length))
      }
      case BeginSentence() => {
        execute(lines.slice(1, lines.length))
      }
      case WriteSentence(s: String) => {
        println(s)
        execute(lines.slice(1, lines.length))
      }
      case WriteSentence(sym: Symbol) => {
        println(getSymVal(sym).toString)
        execute(lines.slice(1, lines.length))
      }
      case WriteSentence(e: ExprSentence) => {
        println(evalExpr(e)().toString())
        execute(lines.slice(1, lines.length))
      }
      case DeclareSentence(sym: Symbol) => {
        declared(sym) = true
        execute(lines.slice(1, lines.length))
      }
      case EndSentence() => {
        if (lines.length > 1) {
          execute(lines.slice(1, lines.length))
        }
      }
      case DoSentence() => {
        execute(lines.slice(1, lines.length))
      }
      case ThenSentence() => {
        execute(lines.slice(1, lines.length))
      }
      case ExprSentence(op: String, lhs: Any, rhs: Any) => {
        evalExpr(head.asInstanceOf[ExprSentence])
        execute(lines.slice(1, lines.length))
      }
      case AssignmentSentence(sym: Symbol, value: Any) => {
        assignment(sym,value)
        execute(lines.slice(1, lines.length))
      }
      case ReadSentence(sym: Symbol) => {
        val value = scala.io.StdIn.readLine()
        assignment(sym, value)
        execute(lines.slice(1, lines.length))
      }
      case ElseSentence() => {
        // this only runs if the contents in the else block should not be run
        // search for the end of the block inside the else and run everything after it
        val loop = new Breaks
        var endIndex = 0
        var sentence = BeginSentence
        loop.breakable {
          for (sentence <- lines) {
            if (sentence.isInstanceOf[EndSentence]) {
              loop.break()
            }
            endIndex += 1
          }
        }
        execute(lines.slice(endIndex + 1, lines.length))
      }
      case IfSentence(cond: ExprSentence) => {
        if (evalExpr(cond)().asInstanceOf[Boolean]) {
          execute(lines.slice(1, lines.length))
        } else {
          val loop = new Breaks
          var elseIndex = 0
          var sentence = BeginSentence
          loop.breakable {
            for (sentence <- lines) {
              if (sentence.isInstanceOf[ElseSentence]) {
                loop.break()
              }
              elseIndex += 1
            }
          }
          execute(lines.slice(elseIndex + 1, lines.length))
        }
      }
      case WhileSentence(cond: ExprSentence) => {
        // search for the end of the loop
        val loop = new Breaks
        var endIndex = 0
        var sentence = BeginSentence
        var block = new mutable.ArrayBuffer[Sentence]
        loop.breakable {
          for (sentence <- lines) {
            if (sentence.isInstanceOf[EndSentence]) {
              block.append(sentence)
              loop.break()
            } else if (sentence.isInstanceOf[WhileSentence]){
              // do nothing
            } else {
              block.append(sentence)
            }
            endIndex += 1
          }
        }
        while (evalExpr(cond)().asInstanceOf[Boolean]) {
          execute(block)
          // println("looped...")
        }
        execute(lines.slice(endIndex + 1, lines.length))
      }
    }
  }

  implicit def symbol2Dec(sym: Symbol) : () => Symbol = () => sym
  implicit def symbol2Expr(sym: Symbol) = Symbol2Expr(sym)
  implicit def expr2Expr(e: ExprSentence) = Expr2Expr(e)
  implicit def symbol2Assignment(sym:Symbol) = Symbol2Assignment(sym)
  implicit def int2Expr(i: Int) = Int2Expr(i)
  implicit def double2Expr(d: Double) = Double2Expr(d)
  implicit def string2Expr(s: String) = String2Expr(s)
}