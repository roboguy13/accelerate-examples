New idea:

Replace the different Cond constructor with

    Cond :: ((a -> Exp Bool) -> (a -> Exp Bool)) -> Exp Bool -> a -> a -> a
            -> Annotation ()

The first argument is a combining function (sidenote: this function has a
semigroup/monoid structure to it).

This almost makes `Annotation` a semigroup, but we would instead need a
(additional, possibly) type that allows nested Haskell expressions with
annotations at each nesting level. If we can make this, we might be able to
collapse many rules that would otherwise need to be written in the future into
something that looks like this:

    annotate a (annotate b x)  ==>  annotate (a <> b) x



Old transformation scratch pad:

if b then t else f
  =>
rep (cond (abs b) (abs t) (abs f))

1.
cond c (recCall t) (recCall f)
  =>
recCall (condRecBoth c t f)

---- Recursive part ----
-- Introduction steps --
2.
cond (c x) (recCall t) f
  =>
recCall (condRecBase c t)

3.
cond (c x) t (recCall f)
  =>
recCall (condRecBase (not . c) f)

-- Combining steps --
4.
recCall (condRecBase c1 (condRecBase c2 x))
  =>
condRecBase (\b -> c1 b ||* c2 b) (recCall x)

5.
recCall (condRecBoth c1 (recCall t) (recCall f))


---- Base case part ----
...
