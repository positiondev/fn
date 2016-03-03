## About

This repository has some helpers that have heavier dependencies than
core [fn](http://hackage.haskell.org/package/fn). In general, things get added
here whenever using libraries on their own conflicts with the design
goals of `Fn`. This could mean that there is a lot of boilerplate to
get them set up, or they are heavily monadic (in which case the code
here serves to make them use more normal functions).
