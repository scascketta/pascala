
import scala.collection.mutable
import scala.language.dynamics

class Pascala extends App {
  abstract sealed class Sentence
  case class ProgramSentence(streams: String) extends Sentence
  case class BeginSentence() extends Sentence
  case class EndSentence() extends Sentence
  case class WriteSentence(s: String) extends Sentence
  case class DeclareSentence(s: Symbol) extends Sentence
  case class IfSentence(cond: Boolean) extends Sentence

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
    def apply(cond: Boolean) = program.append(IfSentence(cond))
  }

  object Then {
    def apply(s: Sentence) = program.append(s)
    def apply(s: Unit) = 1 + 1
  }

  object Else {
    def apply(s: Sentence) = program.append(s)
    def apply(s: Unit) = 1 + 1
  }

  case class Assignment(sym: Symbol) {
    def :=(s: String): Unit = strings(sym) = s
    def :=(i: Int): Unit = ints(sym) = i
    def :=(d: Double): Unit = doubles(sym) = d
    def :=(b: Boolean): Unit = bools(sym) = b
  }

  case class EvalFunction(lhs: Symbol) {
    def +(rhs: Int): Function0[Int] = () => ints(lhs) + rhs
    def +(rhs: Double): Function0[Double] = () => doubles(lhs) + rhs
    def +(rhs: String): Function0[String] = () => strings(lhs) + rhs
    def +(rhs: Symbol): Function0[Any] = {
      if (ints.contains(rhs)) {
        () => ints(lhs) + ints(rhs)
      } else if (doubles.contains(rhs)) {
        () => doubles(lhs) + doubles(rhs)
      } else {
        () => strings(lhs) + strings(rhs)
      }
    }


//    def >(rhs: Int): Boolean = ints(lhs) > rhs
//    def >=(rhs: Int): Boolean = ints(lhs) >= rhs
//    def ==(rhs: Int): Boolean = ints(lhs) == rhs
//    def <(rhs: Int): Boolean = ints(lhs) < rhs
//    def <=(rhs: Int): Boolean = ints(lhs) <= rhs
//    def <>(rhs: Int): Boolean = ints(lhs) != rhs
//    def and(rhs: Boolean): Boolean = bools(lhs) && rhs\
//    def and(rhs: Symbol): Boolean = bools(lhs) && bools(rhs)
//    def or(rhs: Boolean): Boolean = bools(lhs) || rhs
//    def or(rhs: Symbol): Boolean = bools(lhs) || bools(rhs)
  }

  object Program {
    def apply(streams: String) = program.append(ProgramSentence(streams))
  }

  object Write {
    def apply(s: String) = program.append(WriteSentence(s))
    def apply(i: Int) = program.append(WriteSentence(i.toString))
    def apply(d: Double) = program.append(WriteSentence(d.toString))
    def apply(b: Boolean) = program.append(WriteSentence(b.toString))
    def apply(fn: Function0[Any]) = program.append(WriteSentence(fn().toString))
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
      }
    }
  }



  implicit def symbol2Val(sym: Symbol) = EvalFunction(sym)
  implicit def symbol2Assignment(sym:Symbol) = Assignment(sym)
}
