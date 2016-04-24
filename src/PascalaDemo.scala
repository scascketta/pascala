
object PascalaDemo extends Pascala {
  Program ("HelloWorld(stdio)")

  Var ('count)
  Var ('count2)

  Begin
  'count := 20
  'count2 := 40

  If('count < 21)
  Then(Writeln("count is < 10"))
  Else(Writeln("count is >= 10"))

  Writeln('count)

  End

  RUN
}