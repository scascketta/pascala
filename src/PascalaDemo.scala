
object PascalaDemo extends Pascala {
  Program ("HelloWorld(stdio)")

  Var ('count)
  Var ('count2)

  Begin
  'count := 20
  'count2 := 40

  If('count < 19); Then
    Begin
      Writeln("if is true")
      Writeln("if is true - the sequel")
    End
  Else
    Begin
      Writeln("if not true")
      Writeln("if not true - the sequel")
      If('count < 19); Then
        Begin
          Writeln("if is true")
          Writeln("if is true - the sequel")
        End
      Else
        Begin
          Writeln("if not true")
          Writeln("if not true - the sequel")
        End
    End


  Writeln('count)

  End

  RUN
}