object PascalaTest extends Pascala {

  def runIntTests(): Unit = {
    Program ("HelloWorld(stdio)")
    Var ('count)
    Var ('count2)
    Begin
    'int := 20
    'int2 := 40

    Writeln("------------------")
    Writeln("TEST INTEGER MATHS")
    Writeln("------------------")
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

    Writeln("Should be 800")
    Writeln('int * 'int2)
    Writeln("")

    Writeln("Should be 4")
    Writeln('int / 5)
    Writeln("")

    Writeln("Should be 0")
    Writeln('int / 'int2)
    Writeln("")

    Writeln("Should be 2")
    Writeln('int2 / 'int)
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

    Writeln("-----------------")
    Writeln("TEST DOUBLE MATHS")
    Writeln("-----------------")
    Writeln("")

    Writeln("Should be 5.0")
    Writeln('double + 'double2)
    Writeln("")

    Writeln("Should be -2.0")
    Writeln('double - 'double2)
    Writeln("")

    Writeln("Should be 5.25")
    Writeln('double * 'double2)
    Writeln("")

    Writeln("Should be 0.4285...")
    Writeln('double / 'double2)
    Writeln("")

    End

    RUN
  }

  def runStringTests(): Unit = {
    Program ("HelloWorld(stdio)")
    Var ('hello)
    Var ('world)
    Begin
    'hello := "Hello, "
    'world := "World!"

    Writeln("-----------------")
    Writeln("TEST STRING MATHS")
    Writeln("-----------------")
    Writeln("")

    Writeln("Should be Hello,")
    Writeln('hello)
    Writeln("")

    Writeln("Should be Hello, World!")
    Writeln('hello + 'world)
    Writeln("")

    End

    RUN
  }

  def runConditionalTests(): Unit = {
    Program ("HelloWorld(stdio)")
    Var ('x)
    Var ('y)

    Begin
    'x := 10
    'y := 20

    Writeln("----------------------")
    Writeln("TEST CONDITIONAL LOGIC")
    Writeln("----------------------")
    Writeln("")

    Writeln("x is 10")
    Writeln("x < 10")
    If('x < 10) ; Then
    Begin
    Writeln("was true")
    End
    Else
    Begin
    Writeln("was false")
    End
    Writeln("")

    Writeln("x is 10")
    Writeln("x > 10")
    If('x > 10) ; Then
    Begin
    Writeln("was true")
    End
    Else
    Begin
    Writeln("was false")
    End
    Writeln("")

    Writeln("x is 10")
    Writeln("x >= 10")
    If('x >= 10) ; Then
    Begin
    Writeln("was true")
    End
    Else
    Begin
    Writeln("was false")
    End
    Writeln("")

    Writeln("x is 10")
    Writeln("x <= 10")
    If('x <= 10) ; Then
    Begin
    Writeln("was true")
    End
    Else
    Begin
    Writeln("was false")
    End
    Writeln("")

    Writeln("x is 10")
    Writeln("x === 10")
    If('x === 10) ; Then
    Begin
    Writeln("was true")
    End
    Else
    Begin
    Writeln("was false")
    End
    Writeln("")

    Writeln("x is 10")
    Writeln("x <> 10")
    If('x <> 10) ; Then
    Begin
    Writeln("was true")
    End
    Else
    Begin
    Writeln("was false")
    End
    Writeln("")

    Writeln("x is 10")
    Writeln("y is 20")
    Writeln("x < y")
    If('x < 'y) ; Then
    Begin
    Writeln("was true")
    End
    Else
    Begin
    Writeln("was false")
    End
    Writeln("")

    Writeln("x is 10")
    Writeln("y is 20")
    Writeln("x > y")
    If('x > 'y) ; Then
    Begin
    Writeln("was true")
    End
    Else
    Begin
    Writeln("was false")
    End
    Writeln("")

    Writeln("x is 10")
    Writeln("y is 20")
    Writeln("x <= y")
    If('x <= 'y) ; Then
    Begin
    Writeln("was true")
    End
    Else
    Begin
    Writeln("was false")
    End
    Writeln("")

    Writeln("x is 10")
    Writeln("y is 20")
    Writeln("x >= y")
    If('x >= 'y) ; Then
    Begin
    Writeln("was true")
    End
    Else
    Begin
    Writeln("was false")
    End
    Writeln("")

    Writeln("x is 10")
    Writeln("y is 20")
    Writeln("x === y")
    If('x === 'y) ; Then
    Begin
    Writeln("was true")
    End
    Else
    Begin
    Writeln("was false")
    End
    Writeln("")

    Writeln("x is 10")
    Writeln("y is 20")
    Writeln("x <> y")
    If('x <> 'y) ; Then
    Begin
    Writeln("was true")
    End
    Else
    Begin
    Writeln("was false")
    End
    Writeln("")

    End

    RUN
  }

  def runLoopTests(): Unit = {
    Program ("HelloWorld(stdio)")
    Var ('x)

    Begin

    Writeln("----------------")
    Writeln("TEST WHILE LOOPS")
    Writeln("----------------")
    Writeln("")

    'x := 1
    Writeln("x is 1")
    Writeln("should print nothing")
    While('x < 1) ; Do
    Begin
    Writeln('x)
    'x := 'x + 1
    End
    Writeln("")

    'x := 1
    Writeln("x is 1")
    Writeln("should print 1 to 10 by 1")
    While('x <= 10) ; Do
    Begin
    Writeln('x)
    'x := 'x + 1
    End
    Writeln("")

    'x := 2
    Writeln("x is 2")
    Writeln("should print 2 to 10 by 2")
    While('x <= 10) ; Do
    Begin
    Writeln('x)
    'x := 'x + 2
    End
    Writeln("")

    End

    RUN
  }

  runIntTests()
  runDoubleTests()
  runStringTests()
  runConditionalTests()
  runLoopTests()
}