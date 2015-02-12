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
16. A pattern of the form `a@b` can be used to match a value against the pattern `b` but also give the name `a` to the entire value being matched for use in the corresponding expression.
17. When choosing between `foldl` and `foldr`, default to `foldr` and require a really compelling reason for departing from that default; `foldl` can be problematic at times.
18. When writing a list comprehension, You can use pattern matching on the left hand side of the `<-`.
19. "It is good Haskell practice to avoid partial functions as much as possible. Actually, avoiding partial functions is good practice in any programming language—but in most of them it’s ridiculously annoying. Haskell tends to make it quite easy and sensible." (Source: CIS 194, lesson 3) Therefore, avod using/writing partial functions which can crash, especially `head`, `tail`, `init`, `last`, and `(!!)`. Instead, use total functions and/or "crash free" functions.
20. In re type signatures: "The goal is to have the types tell us as much as possible about the behavior of functions. ... if some condition is really guaranteed, then the types ought to reflect the guarantee! Then the compiler can enforce your guarantees for you. ... You might think doing such things is only for chumps who are not coding super-geniuses like you. Of course, you would never make a mistake like passing an empty list to a function which expects only non-empty ones. Right? Well, there’s definitely a chump involved, but it’s not who you think." (Source: CIS 194, lesson3)
21. ***ALWAYS*** write out commentary restating the requirements of the code; this includes descriptions of each function.
22. Use maps, folds, and list comprehensions, to avoid direct recursion and iteration.
23. Avoid direct recursion as much as practical, if not as much as possible, unless that recursion comes in the form of a library function.
24. A "higher order function" is a function which takes a function as a parameter, returns a function as a return value, or both. (Source: "Learn You a Haskell for Great Good", chapter 6)
25. A function declared as `a :: b -> c -> d -> e` is the same as a function declared as `a :: b -> (c -> (d -> e))`.
26. Infix functions can also be partially applied by using sections by surrounding the infix function with parentheses and only supplying a parameter on one side, creating a function taking one parameter and applying it to the side missing an operand.
27. The left hand side of the pipe character in a list comprehension can contain more than simple expressions; it can contain function calls or even another list comprehension.
28. "The most common use case with `flip` is calling it with just the function parameter and then passing the resulting function on to a map or a filter." (Source: "Learn You a Haskell for Great Good", chapter 6)
29. Use lambdas when you want to make it explicit your function is mainly meant to be partially applied and passed to a function as a parameter.
30. The type of the accumulator value and the end result is always the same when dealing with folds.  Remember that fact if You ever don't know what to use as a starting value; it'll give You some idea.
31. The left fold's binary function has the accumulator as the first parameter and the current value as the second one (e.g., `\acc x -> ...`) while the right fold's binary function has the current value as the first parameter and the accumulator as the second one (e.g., `\x acc -> ...`).
32. The `++` function is much more expensive than `:` because for `xs ++ ys`, one `:` is done for every element in `xs`, according to `glguy` on the `haskell-beginners` IRC channel.
33. Right folds work on infinite lists; left folds do not.
34. Folds, maps, and filters, are some of the most, if not the most useful types of functions in functional programming.
35. `foldl` and `foldr` work fine with empty lists; `foldl1` and `foldr1` do not and may be advantageous where empty lists do not make sense.
36. Use a scan if You want to see how a fold progresses.
37. `takeWhile` works on infinite lists; `filter` does not.
38. `filter` takes a predicate and a list and returns the list of elements which satisfy the predicate.
39. Use `(.)` and `($)` to reduce the need for parentheses but be sure to not over-do it or You'll reduce readability.
40. Use `let` bindings to make the code even more readable.
