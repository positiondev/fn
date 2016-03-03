* 0.3.0.0 Daniel Patterson <dbp@dbpmail.net> 2016-3-2

  - Switch Heist to use StateT rather than ReaderT, so splices can
    update context.
  - Add support for compiled heist.

* 0.2.0.1 Daniel Patterson <dbp@dbpmail.net> 2016-1-20

  - Fix for GHC 7.8.4, which cabal file said would work, but didn't.

* 0.2.0.0 Daniel Patterson <dbp@dbpmail.net> 2015-11-5

  - Add `heistServe`, which serves templates according to path.
  - Remove `heistLens` and `setHeist` from `HeistContext` type class,
    as they aren't used.

* 0.1.1.0 Daniel Patterson <dbp@dbpmail.net> 2015-10-30

  - Add `tag'`, which builds splices without atttributes.

* 0.1.0.0 Daniel Patterson <dbp@dbpmail.net> 2015-10-26

  - Initial release.
