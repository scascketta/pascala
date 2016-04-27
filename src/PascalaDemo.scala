object PascalaDemo extends Pascala {
  Program ("HelloWorld(stdio)")

  Var('count)

  Begin
  Writeln("Provide a value for count:")
  Readln('count)
  'words := "hello"
  'doubleval := 12.86

  Writeln('count)

  If('count === "20") ; Then
    Begin
      Writeln('words + " world")
    End
  Else
    Begin
      Writeln("was false")
    End



  End

  RUN
}