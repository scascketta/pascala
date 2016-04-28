object PascalaDemo extends Pascala {
  Program ("HelloWorld(stdio)")

  Var('count)

  Begin
  Writeln("Provide a value for count:")
  'count := 1
  'words := "hello"
  'doubleval := 12.86

  Writeln('count)

  If('count === 20) ; Then
    Begin
      Writeln('words + " world")
    End
  Else
    Begin
      Writeln("was false")
    End

  While('count < 10) ; Do
    Begin
      Writeln('count)
      'count := 'count + 1
    End

  End

  RUN
}