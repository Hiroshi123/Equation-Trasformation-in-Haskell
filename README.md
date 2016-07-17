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

1. String input (Polish Notation)
2. List of Haskell Data type
3. Division of left side of to equal sign and right side of it
4. Making a Tree
5. calculation by pattern matching 
* removing branch -> simple calculation
6. finally : inseting branch -> transposition

##Operation Details (when it contains variable on either leaf)
#### from priority higher to lower
1. inversion :: (subtraction -> addition,division->multiplication)
2. target term selection :: which term is aimed to be merged 
3. commutativity (addition,multiplication) -> branch exchange, rotation
4. associativity (addition,multiplication) -> rotation
5. distribution  (clause which has multiplication & addition) -> rotation & insertion

##target term selection
* e.g. given (3 + x) + 4   = 10, there are three terms in which two constant terms, namely 3 and 4. They need to be merged.
* e.g. given (x + 2) + 2*x = 10, there are three terms in which two terms which shares same degree, namely x and 2*x. They need to be merged.
* e.g. given (x + y) + 4   = 10, there are three terms in which no terms shared same degree. 


##commutativity
#### character
* order of terms is changed
* work under two items in between <+> || <x>
#### example
* 4+(x+3) -> 4+(3+x) :: given target which should be merged is 3

##associativity (order of term is not changed works only in two addition, or two multiplication)
#### character
* order of terms is not changed!
* work under three items in between 1st element  <+> 2nd <+> 3rd || 1st <*> 2nd <*> 3rd
#### example
* 4+(3+x) -> (4+3)+x) :: given target which should be merged is 3


note 
* I am a Haskell beginner -> No monad, a lot of ()
* suppose to have web interface.
* Day by Day Updated