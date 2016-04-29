object PascalaDemo extends Pascala {
  Program ("ApproxPi()")

  Var('iterations)
  Var('squareLen)
  Var('radius)
  Var('centerX)
  Var('centerY)
  Var('i)

  Var('pointX)
  Var('pointY)

  Var('pi)

  Var('numInCircle)

  Begin

  'squareLen := 2.0
  'radius := 'squareLen div 2.0
  'centerX := 'squareLen div 2.0
  'centerY := 'squareLen div 2.0
  'numInCircle := 0.0

  Writeln("Enter number of iterations to run: ")
  Readln('iterations)
  Writeln("# of iterations:")
  Writeln('iterations)

  'i := 0.0
  While('i < 'iterations) ; Do
    Begin
      Rand('pointX, 'squareLen)
      Rand('pointY, 'squareLen)

      Var('xSquared)
      'xSquared := 'pointX * 'pointX

      Var('ySquared)
      'ySquared := 'pointY * 'pointY

      Var('distSquared)
      'distSquared := 'xSquared + 'ySquared

      Var('dist)
      Sqrt('dist, 'distSquared)

      If ('dist <= 'radius); Then
        Begin
          'numInCircle := 'numInCircle + 1.0
        End
      Else
        Begin
        End

      'i := 'i + 1.0
    EndWhile

  'pi := ('numInCircle div 'iterations) * 4.0
  Writeln("Approximation of pi:")
  Writeln('pi)

  End

  RUN
}