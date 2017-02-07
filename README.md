To use these transformations you need
[HERMIT](http://hackage.haskell.org/package/hermit) installed.

There are currently two examples (neither of the corresponding transformations
are fully functional at the moment):

  - In the root directory, there is a Collatz sequence example (this is in
    `Collatz.hs`). The corresponding transformation is in `AccTransform.hss`.
    You can run this example transformation with:

        % hermit Collatz.hs AccTransform.hss +Main

  - In the `Fact/` directory, there is a factorial example (this is in
    `Fact/Fact.hs`). The corresponding transformation is in `Fact/Fact.hss`.
    Parts of the transformation use `Fact/Iter.hs`, which is (in structure) a
    Church-encoded `Either`.
    This transformation can be run with:

        % cd Fact
        % hermit Fact.hs +Main Fact.hss

