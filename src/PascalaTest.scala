
object PascalaTest extends Pascala {

  def runIntTests(): Unit = {
    Program ("HelloWorld(stdio)")
    Var ('count)
    Var ('count2)
    Begin
    'int := 20
    'int2 := 40

    Writeln("TEST INTEGER MATH")
    Writeln("")

    Writeln("Should be 20")
    Writeln('int)
    Writeln("")

    Writeln("Should be 22")
    Writeln('int + 2)
    Writeln("")

    Writeln("Should be 60")
    Writeln('int + 'int2)
    Writeln("")

    Writeln("Should be 10")
    Writeln('int - 10)
    Writeln("")

    Writeln("Should be -20")
    Writeln('int - 'int2)
    Writeln("")

    Writeln("Should be 100")
    Writeln('int * 5)
    Writeln("")

    Writeln("Should be 4")
    Writeln('int div 5)
    Writeln("")

    End

    RUN
  }

  def runDoubleTests(): Unit = {
    Program ("HelloWorld(stdio)")
    Var ('count)
    Var ('count2)
    Begin
    'double := 1.5
    'double2 := 3.5
  }

  runIntTests()
}