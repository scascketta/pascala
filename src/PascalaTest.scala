
object PascalaTest extends Pascala {
  Program ("HelloWorld(stdio)")

  Var ('count)
  Var ('count2)

  Begin
  'count := 20
  'count2 := 40

  Write (2)
  Write ('count + 5 div 'count)
  Write ('count + 3 - 5 div 'count2 div 4)

  End

  RUN
}