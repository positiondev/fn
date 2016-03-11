* 0.3.0.1 Daniel Patterson <dbp@dbpmail.net> 2016-3-11

  - Change repository location, copyright.

* 0.3.0.0 Daniel Patterson <dbp@dbpmail.net> 2016-3-2

  - Switch Heist to use StateT rather than ReaderT, so splices can
    update context.
  - Add support for compiled heist.
  - Add support for digestive functors. Note: current (significant)
    limitation is that if you have _already_ parsed the request body
    with `!=>` then we can only use post params and not files. This is
    because `Fn` parses them to lazy bytestrings, but digestive
    functors wants them in temporary files, and for now, we're not
    converting (it could be done). But, the expectation is that if you
    are using digestive functors, you won't ever be parsing post
    bodies with `!=>`.

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
