# Forth in scheme #

Just a basic Forth-like implementation in chicken scheme.

### Features: ##
  - Ability to define new words with :
  - 64KB Data Stack

### Building - OSX

Install homebrew if you don't have it already, then chicken scheme, then compile.

    $ brew install chicken
    $ csc forth.scm -o forth
    
Then run the Forth implementation!

    £ ./forth

### Useage ###

Its pretty basic, but has the following primitives:

    :            Define new word in dictionary
    +            Add top two items of stack, return answer on top of stack
    -            Subtract top two items of stack, return asnwer on top of stack
    *            Multiply top two items on stack, return answer on top of stack
    /            Divide top two items on stack, return answer on top of stack
    =?           Check if top to items on stack are equal, return #t if they are, return #f if not
    DUP          Duplicate top item on stack
    DISP         Display the top item on the stack
    IF/ELSE/THEN Conditionals, see example below
    PRINT        Prints ASCII value of top of stack
    ?BRANCH      Used in conditionals
    BRANCH       Used in conditionals


### Writing Programs ###

So you want to write programs in Forth? Forth is a stack based language. So to add two numbers, perform the following:

    > (1 2 +)
    3>

### Something more complicated? ###

Okay.. Defining your own words(functions):

    > (: ZERO? 0 =? IF #t ELSE #f THEN)
    > (0 ZERO? DISP)
    #t>

    > (: DOUBLE 2 *)
    > (5 DOUBLE)
    > (DISP)
    10>
    
Have fun!