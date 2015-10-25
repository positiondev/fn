## Fn (eff-enn) - a functional web framework.

> Or, how to do away with the monad transformers, and just use plain
> functions.

## Example

See the example in `example` for how to use this library.

## Recommended Pairings

Part of the design of `Fn` is that you won't need a suite of `fn-foo`
libraries that generally serve to adapt the functions from `foo` to
the monad transformer stack of the web framework of choice. But, it's
still helpful to know what are common tools that are well designed and
tested, so here are a list (those marked with `[*]` are used in the
example application included in this repository):

- [heist](http://hackage.haskell.org/package/heist)`[*]`: a wonderful
  templating system that is both really simple (the templates are just
  html) and powerful (any html tag can be bound to run haskell code).
- [warp](http://hackage.haskell.org/package/warp)`[*]`: perhaps obvious,
  but you will need to choose an HTTP server to use with your `Fn`
  application, and `warp` is the defacto standard for applications that
  use the `WAI` interface that `Fn` does.
- [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)`[*]`:
  a well designed simple interface to PostgreSQL; ofter the lower
  level way to interact with the database (setting up connections,
  etc), if you use a higher level, safer abstraction like `opaleye`
  for actual queries. Use it with
  [resource-pool](https://hackage.haskell.org/package/resource-pool)
  to have it manage many connections.
- [opaleye](https://hackage.haskell.org/package/opaleye): a type-safe
  composable way to write database queries against PostgreSQL.
- [hedis](https://hackage.haskell.org/package/hedis)`[*]`: a full-featured
  client for the key-value store Redis.
- [logging](https://hackage.haskell.org/package/logging)`[*]`: a simple
  library for writing log messages, which allow you to change the
  logging level and suppress some subset of messages.
- [hspec](https://hackage.haskell.org/package/hspec)`[*]`: a full-featured
  testing framework. Use with
  [hspec-wai](https://hackage.haskell.org/package/hspec-wai).
- [wai-session](https://hackage.haskell.org/package/wai-session)`[*]`:
  Combine with something like
  [wai-session-clientsession](https://hackage.haskell.org/package/wai-session-clientsession)
  to store session data in encrypted cookies (like, who a user is
  logged in as).
