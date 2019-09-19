# Changelog
`coercible-utils` uses [PVP Versioning][PVP].

## [0.2.0]

* Install a missing fundep that prevented things like `pack . pack`
  from typechecking.

* Redesign the `Similar` class. Similarity is now transitive:

  ```haskell
  (Similar a b, Similar b c) => Similar a c
  ```

  It's also reflexive under a certain constraint:

  ```haskell
  Similar a b => (Similar a a, Similar b b)
  ```

## [0.1.0] – 2019-09-07

* Improve type inference via a new generic `Newtype` class. ([#14], [#16])
* Improve type signatures to point out which arguments are used only to
  direct type inference. ([#13])


## [0.0.0] – 2018-06-29
Initial release.


[Unreleased]: https://github.com/sjakobi/coercible-utils/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/sjakobi/coercible-utils/compare/v0.0.0...v0.1.0
[0.0.0]: https://github.com/sjakobi/coercible-utils/releases/tag/v0.0.0

[#13]: https://github.com/sjakobi/coercible-utils/pull/13
[#14]: https://github.com/sjakobi/coercible-utils/pull/14
[#16]: https://github.com/sjakobi/coercible-utils/pull/16

[PVP]: https://pvp.haskell.org

