# Whilelang Available Expression Analyse


> A small programming language made with [Scala](https://scala-lang.org) and [ANTLR](https://antlr.org).

This is a simple programming language that has only one loop instruction (while) and a single type (integer).

## Examples
Here are some examples:

Hello World
````ruby
print "Hello World"
````

Sum of two numbers
````ruby
print "Enter the first number:";
a := read;
print "Enter the second number:";
b := read;
sum := a + b;
print "The sum is: ";
print sum
````

Fibonacci Sequence
````ruby
print "Fibonacci Sequence";
a := 0;
b := 1;
while b <= 1000000 do {
  print b;
  b := a + b;
  a := b - a
}
````

## Grammar

The formal syntax is as follows (ANTLR syntax):

````antlr
grammar Whilelang;

program : seqStatement;

seqStatement: statement (';' statement)* ;

statement: ID ':=' expression                          # attrib
         | 'skip'                                      # skip
         | 'if' bool 'then' statement 'else' statement # if
         | 'while' bool 'do' statement                 # while
         | 'print' Text                                # print
         | 'print' expression                          # write
         | '{' seqStatement '}'                        # block
         ;

expression: INT                                        # int
          | 'read'                                     # read
          | ID                                         # id
          | expression '*' expression                  # binOp
          | expression ('+'|'-') expression            # binOp
          | '(' expression ')'                         # expParen
          ;

bool: ('true'|'false')                                 # boolean
    | expression '=' expression                        # relOp
    | expression '<=' expression                       # relOp
    | 'not' bool                                       # not
    | bool 'and' bool                                  # and
    | '(' bool ')'                                     # boolParen
    ;

INT: ('0'..'9')+ ;
ID: ('a'..'z')+;
Text: '"' .*? '"';
Space: [ \t\n\r] -> skip;
````
---

## Compiling & Running

To compile you need to install [sbt](https://www.scala-sbt.org/). 
````shell
$ sbt
sbt> clean
sbt> compile

# To run the interpreter
sbt> runMain whilelang.interpreter.main sum.while
````
