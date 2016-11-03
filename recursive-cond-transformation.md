# New transformation idea:


## General concept and context

For some given a recursive function `fun :: D -> C`, we want to separate out the
recursive part, non-recursive part and the conditional that determines when
recursion should stop. This is particularly challenging when `D` and `C` are
different types. We want to end up with

    fun_nonrec (while fun_cond fun_rec initialValue)

Where `fun_nonrec` is a function with *only* the non-recursive (base case) parts
of `fun` and `fun_rec` is *only* the recursive parts of `fun` (with the
recursive calls themselves replaced with `id`). `fun_cond` is a function
that gives `True` if and only if recursion should continue, given the "current"
recursive argument.

For the types to line up, we need

    fun_nonrec :: Exp D -> Exp C
    fun_rec    :: Exp D -> Exp D

    fun_cond   :: Exp D -> Exp Bool

**Note that a type transformation must happen in some form or another when
extracting `fun_rec` from `fun` in the case where `D` and `C` are different
types.** For the particular example in this repository, we have `fun :: (Int,
Int) -> Int`.


## Annotations used by transformation

Given annotations

    recursive :: a -> a

    condAnn ::
      Elt a =>
      (a -> Exp Bool) -> -- Gives True when the `a` input leads to recursion
      Exp Bool ->
      Exp a ->
      Exp a ->
      Exp a

and something to keep track of the argument **type**:

      condTyAnn ::
        Elt a =>
        Proxy b ->
        Exp Bool ->
        Exp a ->
        Exp a ->
        Exp a
      condTyAnn Proxy = cond


Roughly in order that they must be applied:

## Introduction rules:

0.
TODO: See if this makes sense

    fix f arg
      =>
    let __BASE_FN__ = id
    in
    __BASE_FN__ (recursive (f (\x -> recCall (abs x)) arg))

1.

    if b then t else f
      =>
    rep (cond (abs b) (abs t) (abs f))


2.
Recursion in both branches

    cond (c x) (recCall t) (recCall f)
      =>
    condAnn (\z -> z) (c x) (recCall t) (recCall f)

3.
Recursion only in the true branch

    cond (c x) (recCall t) f
      =>
    condAnn (\z -> c z) (c x) (recCall t) (baseCase f)


4.
Recursion only in the false branch

    cond (c x) t (recCall f)
      =>
    condAnn (\z -> not (c z)) (c x) (baseCase t) (recCall f)

5.
No recursion

    cond (c x) t f
      =>
    condAnn (\z -> False) (c x) t f


## Combination rule:

6.

    forall (cf1 :: a -> Exp Bool) ...
    condAnn cf1 c1 (condAnn cf2 c2 t1 f1) (condAnn cf3 c3 t2 f2)
      =>
    condAnn (\z -> cf1 z &&* (cf2 z ||* not (cf3 z)))
            c1
            (condTyAnn (Proxy @a) c2 t1 f1)
            (condTyAnn (Proxy @a) c3 t2 f2)

## Elimination rules (for use in base case)

Eliminate recursive branches

7.
Recursion in both branches

    condAnn cf c (recCall t) (recCall f)
      =>
    elim (condAnn cf c t f)

8.

    condAnn cf c (recCall t) f
      =>
    f

9.

    condAnn cf c t (recCall f)
      =>
    t

## Elimination rules (for use in recursive case)

Eliminate non-recursive branches. We need to keep track of the conditional
function for use in `while`.)

10.

    condAnn cf c (baseCase t) (baseCase f)
      =>
    elim (condAnn cf c t f)

11.

    condAnn cf c (baseCase t) f
      =>
    f

12.

    condAnn cf c t (baseCase f)
      =>
    t

## General elimination rules (to apply globally)

These allow the expressions to be of a more general type (this is necessary to
separate the overall program into a base case and recursive case and be able to
feed the result of the recursive computation into the base case computation).

13.
(This one might not be necessary)

    elim (elim x) => elim x

14.

    condAnn cf c (elim t) (elim f)
      =>
    error "This should never be reached"


15.

    condAnn cf c (elim t) f
      =>
    condAnn cf c (error "This should never be reached") f

16.

    condAnn cf c t (elim f)
      =>
    condAnn cf c t (error "This should never be reached")

## Type generalization (for use in recursive case)

This, combined with the earlier steps, should allow the recursive case to always
have the same input and output type.

Given the functions

    eqAnn :: a ~ b => Proxy (a ~ b) -> a -> a
    eqAnn Proxy x = x

    safeCoerce :: Proxy (a ~ b) -> a -> b
    safeCoerce Proxy x = x

17.

    forall (p :: Proxy argTy) (t :: b) ...
    condTyAnn p c t f
      =>
    eqAnn (Proxy :: Proxy (argTy ~ b))
          (cond c t f)

18.
This one should only need to be run once, at the top-level.

    forall (cf :: a -> Exp Bool) (t :: b)...
    condAnn cf c t f  -- This should be the top-level conditional by now
      =>
    eqAnn (Proxy :: Proxy (argTy ~ b))
          (condAnn c t f)

19.

    forall (p :: Proxy (argTy ~ b)) ...
    eqAnn p (condAnn cf c t f)
      =>
    safeCoerce p (condAnn cf c t f)

## Combine base case computation and recursive computation using `while`

Given a dummy argument:

    dummyArg :: a

20.
TODO: Figure out what should be happening here

    f . safeCoerce p (condAnn cf c t f)
      =>
    f . while cf (\x -> (cond (cf x) t f))

