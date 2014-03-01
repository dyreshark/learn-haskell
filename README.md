# Learn Haskell!
Various mini-projects in Haskell. Many of which duplicate bits of 
code that are already available. 

## Why?
So I have an excuse to become better at Haskell!

## Mini-project list
(In reverse chronological order)

### Test Finder/Runner
in TestMainGen.hs

Code that generates code! (Until I learn how to template haskell/
a better way to do this.) Essentially, TestMainGen will seek out
test files in the current/child directories and build `Main.hs`, 
a file that can be run to run all available tests.

### JSON Parser
in Json.hs, Json/\*. 

A simple library that serializes JSON in a typed manner. Includes 
my first ever attempt to use QuickCheck.

## "Why did you do X this way?"
Probably out of ignorance. Feel free to contact me if there's a better
way to do something. Constructive criticism is always appreciated. :)

## Do you have tests?
Yes! Run `make` (It's more of a "Test everything" tool than a "make me
something useful" tool, due to the nature of the repo)
