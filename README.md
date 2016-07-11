# Equation-Trasformation-in-Haskell

Basic Idea
![my image](arithmeticFormula.png)

Arithmetic formula can be desctibed in tree representation whose top node is equal sign, the rest of node is operator and leaf (edge) is number or variable.

only for equation containing addition and subtraction

String input
-> List of Haskell Data type
-> Division of left side of to equal sign and right side of it
-> Tree
-> Subtraction to addition
-> serialize (back to list)
-> sort by signatures [leftest::operator,middle::number,rightest::variable] <- addition meets both associativity and commutativity
-> expansion of the equation

note 
would not work out in multiplication so far...
I am a Haskell beginner -> No monad, a lot of ()
