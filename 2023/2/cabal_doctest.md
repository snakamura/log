# Use write-ghc-environment-files to run doctest with cabal

Imagine you have a test suite in your cabal file that uses doctest, and you're trying to run it with `cabal test`.

This might fail because doctest cannot find some dependencies. This happens because doctest doesn't look into the cabal file for dependencies when it runs tests.

You can put `write-ghc-environment-files` in your `cabal.project` file to fix this.

```
write-ghc-environment-files: always
```

This makes ghc to write the environment to `.ghc.environment.aarch64-darwin-9.2.5` (the last part depends on your environment), and doctest loads it to find dependencies.

* [Cabal-3: doctest-discover fails for non-boot libraries #6087](https://github.com/haskell/cabal/issues/6087)
* [8. cabal.project Reference](https://cabal.readthedocs.io/en/3.4/cabal-project.html#cfg-field-write-ghc-environment-files)
