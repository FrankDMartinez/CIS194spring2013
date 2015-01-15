CIS194spring2013
================

CIS 194: Introduction to Haskell (Spring 2013 Version)

##Some Notes I have found useful:
1. Use `-fwarn-incomplete-patterns` to get a warning during compilation if a sequence of patterns do not match all of a type's value constructors.
2. "As we have already mentioned, a Haskell implementation checks patterns for matches in the order in which we specify them in our equations. Matching proceeds from top to bottom, and stops at the first success. Equations below a successful match have no effect." -- *Real World Haskell*, chapter 3
3. "From reading the preceding sections, it should now be clear that ***all* of the data types that we define with the `data` keyword are algebraic data types**. Some may have just one alternative, while others have several, but they're all using the same machinery." -- *Real World Haskell*, chapter 3, emphasis added
4. When defining a data type, `deriving (Show)` is required for values of the type to be printable.
5. When defining a data type, `deriving (Eq)` is required for value of the type to be comparable for equality.
6. "Beware enumerating floating point numbers ... Using enumeration notation over floating point numbers can pack more than a few surprises, so if you use it at all, be careful. Floating point behavior is quirky in all programming languages; there is nothing unique to Haskell here." -- *Real World Haskell*, chapter 1
7. Typing `:?` at the `ghci` prompt prints a long help message.
8. Commas in a list are separators and not terminators; ending a list with just a comma will result in a parse error.
9. When You see an error message, ***DON'T PANIC***; when You see an error message You do not understand, ***DON'T PANIC***; just read what is on the screen because *that's why it is there*.
10. Use record syntax by default; require a ***REALLY*** good reason for not doing so. As one Person put it, "Use this [record syntax] like it's going to give You head!"
11. Use `-fwarn-name-shadowing` to get a warning during compilation if a name is shadowed.
12. Variables can be defined at the top level of a source file.
13. Top level declarations may start in any column as long as all subsequent top level declarations do the same.
14. Always adhere to the "offside rule" and avoid explicit structuring.
15. Using a single identifier as a pattern will always match; it will not reference any top level declaration of the same name.
