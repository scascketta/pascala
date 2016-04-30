object PascalaDemo extends Pascala {
  Program ("ApproxPi")

  Var('maxIterations);
  Var('squareLen);
  Var('radius);
  Var('centerX);
  Var('centerY);
  Var('xSquared);
  Var('ySquared);
  Var('distSquared);
  Var('dist);

  Var('pointX);
  Var('pointY);

  Var('numInCircle);
  Var('i);
  Var('currentIterations);

  Var('pi);

  Begin

  'squareLen := 2.0;
  'radius := 'squareLen div 2.0;
  'centerX := 'squareLen div 2.0;
  'centerY := 'squareLen div 2.0;
  'numInCircle := 0.0;

  Writeln("Enter upper bound of maxIterations to run (as a double): ");
  Readln('maxIterations);
  Writeln("# of maxIterations:");
  Writeln('maxIterations);

  'currentIterations := 10.0;
  While('currentIterations <= 'maxIterations) ; Do
  Begin
      Writeln("Current iterations: ");
      Writeln('currentIterations);

      'i := 0.0;
      'numInCircle := 0.0;
      While('i < 'currentIterations) ; Do
      Begin
          Rand('pointX, 'squareLen);
          Rand('pointY, 'squareLen);

          'xSquared := 'pointX * 'pointX;
          'ySquared := 'pointY * 'pointY;
          'distSquared := 'xSquared + 'ySquared;

          Sqrt('dist, 'distSquared);

          If ('dist <= 'radius); Then
            Begin
              'numInCircle := 'numInCircle + 1.0;
            End
          Else
            Begin
            End

          'i := 'i + 1.0;
      End

      'pi := ('numInCircle div 'currentIterations) * 4.0;
      Writeln("Approximation of pi:");
      Writeln('pi);

      'currentIterations := 'currentIterations * 10.0;

  End

  End

  RUN
}