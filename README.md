# Nondeterminism

This provides nondeterministic computations and constraint
satisfactions solvers in Haskell. It implements an `Amb` monad in
which you can perform nondeterministic choices along with a monad
transformer version, `AmbT`. Using this mechanism it builds up
constraint satisfaction with different constraint types like arc
consistency.

# CSP

A simple example which solves Sudoku puzzles, project Euler problem 96.

    solveSudoku :: (Enum a, Eq a, Num a) => [[a]] -> [[a]]
    solveSudoku puzzle = solveCSP $ do
      dvs <- mapM (mapM (\a -> mkDV $ if a == 0 then [1 .. 9] else [a])) puzzle
      mapM_ assertRowConstraints dvs
      mapM_ assertRowConstraints $ transpose dvs
      sequence_ [assertSquareConstraints dvs x y | x <- [0,3,6], y <- [0,3,6]]
      return dvs
          where assertRowConstraints =  mapAllPairsM_ (constraint2 (/=))
                assertSquareConstraints dvs i j = 
                    mapAllPairsM_ (constraint2 (/=)) [(dvs !! x) !! y | x <- [i..i+2], y <- [j..j+2]]

    sudoku3 = [[0,0,0,0,0,0,9,0,7],
               [0,0,0,4,2,0,1,8,0],
               [0,0,0,7,0,5,0,2,6],
               [1,0,0,9,0,4,0,0,0],
               [0,5,0,0,0,0,0,4,0],
               [0,0,0,5,0,7,0,0,9],
               [9,2,0,1,0,8,0,0,0],
               [0,3,4,0,5,9,0,0,0],
               [5,0,7,0,0,0,0,0,0]]

    mapAllPairsM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
    mapAllPairsM_ f []     = return ()
    mapAllPairsM_ f (_:[]) = return ()
    mapAllPairsM_ f (a:l) = mapM_ (f a) l >> mapAllPairsM_ f l

    solveSudoku sudoku3

# Amb

An example which finds Pythagorean triplets up to a certain size, project Euler problem 9.

    pyTriple :: (Num t, Ord t) => t -> Amb r (t, t, t)
    pyTriple n = do a <- anIntegerBetween 1 n
                    b <- anIntegerBetween (a + 1) n
                    c <- anIntegerBetween (b + 1) n
                    when (a*a + b*b /= c*c) fail
                    return (a,b,c)

    length $ allValues $ pyTriple 100000

# Future

 - Allow a randomized execution order for CSPs
 - CSPs don't need use IO internally. ST is enough.
 - Constraint synthesis. Already facilitated by the fact that
   constraints are internally nondeterministic
 - Other constraint types for CSPs, right now only AC is implemented
 - n-ary heterogeneous constraints 
