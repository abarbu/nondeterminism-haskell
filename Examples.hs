{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where
import Amb hiding (tell)
import Control.Monad hiding (fail)
import Prelude hiding (fail,either)
import Control.Applicative
import CSP
import System.IO.Unsafe
import Data.List

-- Tests

test0 :: Monad m => AmbT r m b
test0 = fail

test1 :: (Monad m, Num b) => AmbT r m b
test1 = do b <- aBoolean
           if b then fail else return 1

test2 :: (Monad m, Num b) => AmbT r m b
test2 = do b <- aBoolean
           if b then return 1 else fail

testB1 = do a <- aMemberOf [1,2,3,4]
            return $ a == 4
testB2 = do a <- aMemberOf [1,2,3,4]
            return $ a < 5
testB3 = do a <- aMemberOf [1,2,3,4]
            return $ a > 4

test3 :: Monad m => AmbT r m (Bool, Bool)
test3 = do a <- aBoolean
           b <- aBoolean
           case (a,b) of
             (True,True) -> fail
             (True,False) -> fail
             (False,True) -> return (a,b)
             (False,False) -> return (a,b)

example1 :: (Eq t, Monad m, Num t) => AmbT r m (t, t)
example1 = do x <- amb [1,2,3]
              y <- amb [4,5,6]
              if x*y == 10 then return (x,y) else amb []

example2 :: (Monad m, Num t, Ord t) => AmbT r m (t, t)
example2 = do x <- amb [1,2,3]
              y <- amb [4,5,6]
              if x*y > 10 then return (x,y) else amb []

pyTriple :: (Num t, Ord t) => t -> Amb r (t, t, t)
pyTriple n = do a <- anIntegerBetween 1 n
                b <- anIntegerBetween (a + 1) n
                c <- anIntegerBetween (b + 1) n
                when (a*a + b*b /= c*c) fail
                return (a,b,c)

testC0 = do
  a <- mkDV [1,2,5]
  constraint1 (==2) a
  return a

testC1 = do
  a <- mkDV [1,2,5]
  b <- mkDV [10,4,7]
  constraint2 (>) a b
  return (a,b)

testC2 = do
  a <- mkDV ["1","2","5"]
  b <- mkDV [3,2,7]
  constraint2 (\a b -> read a == b) a b
  return (a,b)

testC = (solveCSP testC0, solveCSP testC1, solveCSP testC2)

-- Project Euler problem 96

-- (define (p96)
--  (define (reflow a)
--   (if (null? a) '() (cons (take 9 a) (reflow (drop 9 a)))))
--  (define (solve p)
--   (let ((dvs (map (lambda (r)
-- 		   (map (lambda (a)
-- 			 (if (= a 0)
-- 			     (create-domain-variable (map-n (lambda (a) (+ 1 a)) 9))
-- 			     (create-domain-variable (list a))))
-- 			r))
-- 		  p)))
--    (define (assert-row-constraints! r)
--     (map-all-pairs (lambda l (assert-constraint! (lambda (a b) (not (= a b))) l)) r))
--    (for-each assert-row-constraints! dvs)
--    (for-each assert-row-constraints! (map-n (lambda (i) (map (lambda (l) (list-ref l i)) dvs)) (length dvs)))
--    (define (assert-square-constraints! i j)
--     (map-all-pairs
--      (lambda l (assert-constraint! (lambda (a b) (not (= a b))) l))
--      (map
--       (lambda (i) (list-ref (list-ref dvs (first i)) (second i)))
--       (cross-product (map-m-n identity i (+ i 2))
-- 		     (map-m-n identity j (+ j 2))))))
--    (assert-square-constraints! 0 0) (assert-square-constraints! 3 0) (assert-square-constraints! 6 0)
--    (assert-square-constraints! 0 3) (assert-square-constraints! 3 3) (assert-square-constraints! 6 3)
--    (assert-square-constraints! 0 6) (assert-square-constraints! 3 6) (assert-square-constraints! 6 6)
--    (reflow (csp-solution (concat dvs) first))))
--  (let* ((f (let loop ((f (read-file "sudoku.txt")))
-- 	    (if (null? f) '() (cons (cdr (take 10 f)) (loop (drop 10 f))))))
-- 	(puzzles (map (lambda (p) (map (lambda (s) (map string->number (map list->string (map list (string->list s))))) p)) f)))
--   (map (lambda (a) (standard-csp-startup) (solve a)) puzzles)))

mapAllPairsM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
mapAllPairsM_ f []     = return ()
mapAllPairsM_ f (_:[]) = return ()
mapAllPairsM_ f (a:l) = mapM_ (f a) l >> mapAllPairsM_ f l

--solveSudoku :: (Enum a, Eq a, Num a) => [[a]] -> [[a]]
solveSudoku puzzle = solveCSP $ do
  dvs <- mapM (mapM (\a -> mkDV $ if a == 0 then [1 .. 9] else [a])) puzzle
  mapM_ assertRowConstraints dvs
  mapM_ assertRowConstraints $ transpose dvs
  sequence_ [assertSquareConstraints dvs x y | x <- [0,3,6], y <- [0,3,6]]
  return dvs
      where assertRowConstraints =  mapAllPairsM_ (constraint2 (/=))
            assertSquareConstraints dvs i j = 
                mapAllPairsM_ (constraint2 (/=)) [(dvs !! x) !! y | x <- [i..i+2], y <- [j..j+2]]

-- -- 1 is quite fast, 3 is very slow
sudoku1 = [[0,0,3,0,2,0,6,0,0],[9,0,0,3,0,5,0,0,1],[0,0,1,8,0,6,4,0,0],[0,0,8,1,0,2,9,0,0],[7,0,0,0,0,0,0,0,8],[0,0,6,7,0,8,2,0,0],[0,0,2,6,0,9,5,0,0],[8,0,0,2,0,3,0,0,9],[0,0,5,0,1,0,3,0,0]]
sudoku3 = [[0,0,0,0,0,0,9,0,7],[0,0,0,4,2,0,1,8,0],[0,0,0,7,0,5,0,2,6],[1,0,0,9,0,4,0,0,0],[0,5,0,0,0,0,0,4,0],[0,0,0,5,0,7,0,0,9],[9,2,0,1,0,8,0,0,0],[0,3,4,0,5,9,0,0,0],[5,0,7,0,0,0,0,0,0]]

p96 :: [(Int, [[Int]])]
p96 = let f = unsafePerformIO $ readFile "sudoku.txt"
      in map (\(g:gs) -> (read $ drop 5 g, solveSudoku $ map (\g -> map (read . (:[])) g) gs))
             $ groupBy (\a b -> not $ isPrefixOf "Grid" b) $ lines f

main = print $ p96
