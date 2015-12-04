* 0.2.1.0 Daniel Patterson <dbp@dbpmail.net> 2015-12-4

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
