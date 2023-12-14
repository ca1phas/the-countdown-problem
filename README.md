Project code for https://youtu.be/CiXDS3bBBUo?si=m0wWOww_40e154fz

# The Countdown Problem

### Description

Find an expression (if exists) that yield a given target number using some/all of the numbers in a given list and the operators +, -, × and/or ÷.

### Rules

1.  All numbers must be positive naturals.<br>

2.  All numbers in the list can be used once only.

### How to use the program

1. Open a terminal window.
2. Type `ghci index.hs` and press Enter.
3. Type `[solution_type] [ns] [n]` and press Enter.<br>
   n = the target number e.g. _765_<br>
   ns = the list of numbers e.g. _[1,3,7,10,25,50]_<br>
   solution*type = <br>
   a. \_solution* (brute force search), <br>
   b. _solution'_ (combining generation and evaluation), <br>
   c. _solution''_ (exploiting algebraic properties)
