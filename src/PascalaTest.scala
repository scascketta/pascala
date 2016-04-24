
object PascalaTest extends Pascala {
  Program ("HelloWorld(stdio)")

  Var ('count)
  Var ('count2)

  Begin
  'count := 20
  'count2 := 5

  Write (2)
  Write ('count div 'count2 * 2)

  End

  RUN
}