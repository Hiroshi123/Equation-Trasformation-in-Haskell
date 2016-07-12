# Equation-Trasformation-in-Haskell

#_Basic Idea_
![my image](arithmeticFormula.png)

* Arithmetic formula can be desctibed in tree representation whose top node is equal sign, the rest of node is operator and leaf (edge) is number or variable.

* Insertion, deletion, rotation of branches can be basic operation of operating equation.


* Principle of its implementation
1. Being faithful to step by step expansion which has no leap like human's cognition do

####So far
only for equation containing addition and subtraction which has just one variable


##Process

String input
1. List of Haskell Data type
2. Division of left side of to equal sign and right side of it
3. Making a Tree
4. calculation by pattern matching 
* removing branch -> simple calculation
* inseting branch -> transposition

note 
* would not work out in multiplication so far...
* I am a Haskell beginner -> No monad, a lot of ()
* Day by Day Updated