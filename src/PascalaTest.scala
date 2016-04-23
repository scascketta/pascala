
object PascalaTest extends Pascala {
  Program ("HelloWorld(stdio)")

  Var ('count)
  Var ('count2)

  Begin
  'count := 10
  'count2 := 22

  Write (2)
  Write ('count + 2 + 2)
  Write ('count + 'count)
  Write ('count + 'count2)

  End

  RUN
}