
import scala.collection.mutable
import scala.language.dynamics
import scala.util.control.Breaks

class Pascala extends App {
  abstract sealed class Sentence
  case class ProgramSentence(streams: String) extends Sentence
  case class BeginSentence() extends Sentence
  case class EndSentence() extends Sentence
  case class WriteSentence(s: String) extends Sentence
  case class DeclareSentence(s: Symbol) extends Sentence
  case class IfSentence(cond: Function0[Boolean]) extends Sentence
  case class ThenSentence() extends Sentence

  var program = new mutable.ArrayBuffer[Sentence]

  var doubles = new mutable.HashMap[Symbol, Double]()
  var ints = new mutable.HashMap[Symbol, Int]()
  var bools = new mutable.HashMap[Symbol, Boolean]()
  var strings = new mutable.HashMap[Symbol, String]()

  // for checking if a variable is declared before being assigned
  var declared = new mutable.HashMap[Symbol, Boolean]()

  def RUN() = execute(program)

  def Begin = program.append(BeginSentence())

  object Var extends Dynamic {
    def apply(sym: Symbol) = program.append(DeclareSentence(sym))
  }

  object If {
    def apply(cond: Function0[Boolean]) = program.append(IfSentence(cond))
  }

  def Then = program.append(ThenSentence())

  case class Assignment(sym: Symbol) {
    def :=(s: String): Unit = strings(sym) = s
    def :=(i: Int): Unit = ints(sym) = i
    def :=(d: Double): Unit = doubles(sym) = d
    def :=(b: Boolean): Unit = bools(sym) = b
  }

  case class EvalSymbol(lhs: Symbol) {
    def +(rhs: Int): Function0[Int] = () => ints(lhs) + rhs
    def +(rhs: Function0[Any]): Function0[Any] = {
      if (ints.contains(lhs)) {
        () => ints(lhs) + rhs().asInstanceOf[Int]
      } else if (doubles.contains(lhs)) {
        () => doubles(lhs) + rhs().asInstanceOf[Double]
      } else {
        () => strings(lhs) + rhs().asInstanceOf[String]
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
      if (ints.contains(lhs)) {
        () => ints(lhs) - rhs().asInstanceOf[Int]
      } else {
        () => doubles(lhs) - rhs().asInstanceOf[Double]
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
      if (ints.contains(lhs)) {
        () => ints(lhs) * rhs().asInstanceOf[Int]
      } else {
        () => doubles(lhs) * rhs().asInstanceOf[Double]
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
      if (ints.contains(lhs)) {
        () => ints(lhs) / rhs().asInstanceOf[Int]
      } else {
        () => doubles(lhs) / rhs().asInstanceOf[Double]
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
      if (ints.contains(lhs)) {
        () => ints(lhs) > rhs().asInstanceOf[Int]
      } else {
        () => doubles(lhs) > rhs().asInstanceOf[Double]
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
      if (ints.contains(lhs)) {
        () => ints(lhs) >= rhs().asInstanceOf[Int]
      } else {
        () => doubles(lhs) >= rhs().asInstanceOf[Double]
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
      if (ints.contains(lhs)) {
        () => ints(lhs) == rhs().asInstanceOf[Int]
      } else if (doubles.contains(lhs)) {
        () => doubles(lhs) == rhs().asInstanceOf[Double]
      } else if (strings.contains(lhs)) {
        () => strings(lhs).equals(rhs().asInstanceOf[String])
      } else {
        () => bools(lhs) == rhs().asInstanceOf[Boolean]
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
      if (ints.contains(lhs)) {
        () => ints(lhs) <= rhs().asInstanceOf[Int]
      } else if (doubles.contains(lhs)) {
        () => doubles(lhs) <= rhs().asInstanceOf[Double]
      } else {
        throw new IllegalStateException("Cannot use <= operator with strings or booleans.")
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
      if (ints.contains(lhs)) {
        () => ints(lhs) < rhs().asInstanceOf[Int]
      } else if (doubles.contains(lhs)) {
        () => doubles(lhs) < rhs().asInstanceOf[Double]
      } else {
        throw new IllegalStateException("Cannot use < operator with strings or booleans.")
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
      if (ints.contains(lhs)) {
        () => ints(lhs) != rhs().asInstanceOf[Int]
      } else if (doubles.contains(lhs)) {
        () => doubles(lhs) != rhs().asInstanceOf[Double]
      } else if (strings.contains(lhs)) {
        () => strings(lhs).equals(rhs().asInstanceOf[String])
      } else {
        () => bools(lhs) != rhs().asInstanceOf[Boolean]
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

  object Writeln {
    def apply(s: String) = program.append(WriteSentence(s))
    def apply(i: Int) = program.append(WriteSentence(i.toString))
    def apply(d: Double) = program.append(WriteSentence(d.toString))
    def apply(b: Boolean) = program.append(WriteSentence(b.toString))
    def apply(fn: Function0[Any]) = program.append(WriteSentence(fn().toString))
    def apply(sym: Symbol) = program.append(WriteSentence(getSymVal(sym).toString))
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

  def End = program.append(EndSentence())

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
      case DeclareSentence(sym: Symbol) => {
        declared(sym) = true
        execute(lines.slice(1, lines.length))
      }
      case EndSentence() => {
        if (lines.length > 1) {
          execute(lines.slice(1, lines.length))
        }
      }
      case ThenSentence() => {
        execute(lines.slice(1, lines.length))
      }
      case IfSentence(cond: Function0[Boolean]) => {
        val loop = new Breaks
        var startIndex = 0
        var sentence = BeginSentence
        loop.breakable {
          for (sentence <- lines) {
            if (sentence.isInstanceOf[EndSentence]) {
              loop.break()
            }
            startIndex += 1
          }
        }

        if (cond()) {
          execute(lines.slice(1, lines.length))
        } else {
          execute(lines.slice(startIndex + 1, lines.length))
        }
      }
    }
  }

  implicit def symbol2Func(sym: Symbol) = EvalSymbol(sym)
  implicit def func2Func(fn: Function0[Any]) = EvalFunction0(fn)
  implicit def symbol2Assignment(sym:Symbol) = Assignment(sym)
}
