# Pascala
Ever wanted to write pseudo-Pascal inside Scala? Well, here it is anyway.

Looking for a neat example of Pascala in action? Check out [PascalaDemo.scala](https://github.com/scascketta/pascala/blob/master/src/PascalaDemo.scala), a Pascala program that approximates π using a Monte Carlo algorithm.

## Usage

The first thing you need to know about Pascala is that it's an *internal* DSL, which means you have to write Pascala inside a Scala program.

For example, a "Hello, world!" program looks like this:

```scala
object HelloWorld extends Pascala {
  Program("HelloWorld");
  Begin
  Writeln("Hello, world!");
  End
}
```

Pascala extends Scala's [`App`](http://www.scala-lang.org/api/2.11.8/#scala.App) trait so you don't need to worry about any of that `main(args: Array[String])` boilerplate.

## Language Features

Here's some example code to explore the Pascala's features

```scala
object Example extends Pascala {
  // Doesn't actually matter what arg you pass to Program
  Program("Example");

  //  Declare variables with Var('symbol)
  //  All variables are Scala symbols
  // You can declare variables before or after the program's Begin statement
  // Pascala supports integers, doubles, strings, and booleans
  Var('i);
  Var('j);

  Var('name);

  //  Wrap your program in Begin-End statements
  Begin
    // Writeln prints to stdout with a newline after your argument
    Writeln("Enter your name:")
    // Readln reads from stdin and assigns what it reads to the symbol passed as an arg
    // It supports parsing ints (e.g. 42), doubles (e.g. 3.14), and strings
    // It first tries to parse as an int, then a double, and then a string
    Readln('name);

    // assignment uses the ':=' token
    'i := 0;

    // Wrap While loops with Begin-End as well
    While('i < 100); Do
    Begin
      // Rand gets a random integer from 0 (inclusive) to the second arg passed (exclusive)
      // this assigns a random int from [0, 100) to the variable 'j
      Rand('j, 100);

      // This assigns the sqrt of 'j (2nd arg) to 'j (1st arg)
      Sqrt('j, 'j);

      // Pascala supports nested loops
      While('j < 100); Do
      Begin
        // Conditionals in Pascala support If-Else statements (also wrapped with Begin-End)
        If('j > 10);
        Then
          Begin
            // Pascala obeys order of operations so this is evaluated as 'j + 1 at runtime
            Writeln('j + 3  * 5 / 15);
          End
        Else
          Begin
            Writeln('i + 3 * 5 / 15);
          End
        'j := 'j + 1
      End
      'i := 'i + 1
    End
  End
}

```


