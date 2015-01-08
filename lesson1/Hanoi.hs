type Peg = String
type Move = (Peg, Peg)

-- given the number of discs and names for the three pegs,
-- returns a list of moves to be performed to move the stack of
-- discs from the first peg to the second
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 from to _ = [(from, to)]
hanoi n from to temporary = (hanoi (n-1) from temporary to) ++ (hanoi 1 from to temporary) ++ (hanoi (n-1) temporary to from)
