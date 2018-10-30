* 0.3.1.1 Tom Murphy <tom@tinybop.com> 2018-10-12

  - Add MINIMAL pragma for RequestContext class

* 0.3.1 Libby Horacek <libby@positiondev.com> 2017-6-13

  - Update base dependencies to support GHC 8.4
  - Add a stack.yml for LTS 12.11
  - Drop support for Stackage < 7 (mostly meaning Heist < 1.0)

* 0.3.0.2 Libby Horacek <libby@positiondev.com> 2017-6-13

  - Update base dependencies to support GHC 8
  - Add a stack.yml for LTS 8.15

* 0.3.0.1.1 David Hartunian <david@positiondev.com> 2017-6-30

  - Add okJson helper for returning JSON strings as text

* 0.3.0.1 Daniel Patterson <dbp@dbpmail.net> 2016-3-11

  - Change repository location, copyright.

* 0.3.0.0 Daniel Patterson <dbp@dbpmail.net> 2016-3-2

  - Don't parse request body by default, to make `Fn` play well with
    others (that want to parse the body themself).
  - Add `!=>` connective that is like `==>`, but parses the request
    body. If you don't use `!=>`, patterns with `file` and `files`
    will fail. Also, `param` will only get query parameters.
  - Add `Route` type alias for the type of `pattern ==> handler`. This
    is partly for convenience and partly to make upgrades easier (in
    the event that the types change).
  - Change `FromParam` class to take a list of all parameters matching
    a given name, which allows us to implement a `Maybe` instance, a
    list instance, and make `paramMany` redundant (though currently
    left in, for compatibility). This also makes the ergonomics of
    using optional parameters better.
  - Fix bug where `staticServe` would allow you to break out of
    directory specified with `..`.

* 0.2.0.2 Daniel Patterson <dbp@dbpmail.net> 2016-1-20

  - Fix for GHC 7.8, which cabal file said would work, but didn't.

* 0.2.0.1 Daniel Patterson <dbp@dbpmail.net> 2015-12-4

  - Fix bug in url routing where "/foo/bar", "/foo/bar/", and
    "/foo//bar" were all treated differently.

* 0.2.0.0 Daniel Pattersion <dbp@dbpmail.net> 2015-11-5

  - Changed to having our own `FnRequest` type, which is a WAI
    `Request` and the results of parsing the body for contents, since
    we need to be able to do that once and thread it through.
  - Add `file` and `files` matchers, which match against and pass file
    uploads to handlers.
  - Add `staticServe` to serve static files based on path.

* 0.1.4.0 Daniel Pattersion <dbp@dbpmail.net> 2015-11-4

  - Move `ctxt` back to first parameter passed to handlers, via more
    continuations.

* 0.1.3.1 Daniel Pattersion <dbp@dbpmail.net> 2015-10-31

  - Add `method` matcher to match against HTTP method.

* 0.1.3.0 Daniel Patterson <dbp@dbpmail.net> 2015-10-30

  - Allow nested calls to `route`, by changing `Request` in
    `ctxt`. This necesitated changing it so that the `ctxt` is passed
    to handlers _last_, instead of first, because we need to have
    completed matching before we can change the request.
  - Add `anything` route matcher that matches anything.
  - Add `paramMany` matcher that returns a list of values for the
    given query param.
  - Change `param` to fail if more than one value is in query string.

* 0.1.2.0 Daniel Pattersion <dbp@dbpmail.net> 2015-10-27

  - Rename `paramOptional` to `paramOpt`, to match `fn-extra`'s `Heist`
    naming of `attr` and `attrOpt`.
  - Remove `paramPresent`, because you
    can get that behavior by parsing to `Text`.

* 0.1.1.0 Daniel Patterson <dbp@dbpmail.net> 2015-10-26

  - Rename `Param` class to `FromParam`.

* 0.1.0.0 Daniel Patterson <dbp@dbpmail.net> 2015-10-25

  - Initial release.
