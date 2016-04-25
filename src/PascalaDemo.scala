
object PascalaDemo extends Pascala {
  Program ("HelloWorld(stdio)")

  Var ('count)
  Var ('count2)

  Begin
  'count := 20
  'count2 := 40

  If('count < 19)
  Then
  Begin
  Writeln("if is true")
  End


  Writeln('count)

  End

  RUN
}