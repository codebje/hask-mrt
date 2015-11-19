## Haskell MRT

`hask-mrt` is a library for parsing Multi-Threaded Routing Toolkit (MRT) export
files, of the kind you might find on the [RouteViews][RV] archive.

Building should be done using `stack`, but you can use `cabal` if you really
want.

[Documentation] for the latest build is available.

[![Build status](https://travis-ci.org/codebje/hask-mrt.svg)](https://travis-ci.org/codebje/hask-mrt)

### Support for data types

#### MRT record types

 - `TableDumpV2` records
 - `PeerIndex` records (ignored)

All other record types are currently an error.  `PeerIndex` tables are currently
ignored - `TableDumpV2` records don't contain peer information.

#### BGP attributes

 - BGP Origin
 - Local Preference
 - Atomic Aggregate
 - Aggregator
 - Communities

All other BGP attributes are stored as an unparsed `ByteString`.

### Specifications

[RFC 6396] - Multi-Threaded Routing Toolkit (MRT) Routing Information Export Format


[RFC 6396]: https://tools.ietf.org/html/rfc6396
[RV]: http://routeviews.org/
[Documentation]: http://λ.bje.id.au/hask-mrt/
