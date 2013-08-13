# Nondeterminism

This provides nondeterministic computations in Haskell. It implements
an `Amb` monad in which you can perform nondeterministic choices along
with a monad transformer version, `AmbT`.

## Amb

An example which finds Pythagorean triplets up to a certain size, project Euler problem 9.

    pyTriple :: (Num t, Ord t) => t -> Amb r (t, t, t)
    pyTriple n = do a <- anIntegerBetween 1 n
                    b <- anIntegerBetween (a + 1) n
                    c <- anIntegerBetween (b + 1) n
                    when (a*a + b*b /= c*c) fail
                    return (a,b,c)

    length $ allValues $ pyTriple 100000

## Future

 - Docs!
