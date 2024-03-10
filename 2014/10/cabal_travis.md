# Using cabal-install-1.20 on Travis-CI

Though Travis-CI uses cabal-install-1.18, I wanted to use 1.20 to use `--allow-newer` option. After browsing some documents, the easiest way seems to use [ppa:hvr/ghc](https://launchpad.net/~hvr/+archive/ubuntu/ghc). You can install cabal-install-1.20 from this ppa, as well as alex and happy.

One thing to note is that you need to install [Cabal library](http://hackage.haskell.org/package/Cabal) explicitly before install packages. Otherwise, it fails to configure packages with not-so-clear errors.

My .travis.yml looks like this.

    language: haskell
    ghc:
      - 7.8

    before_install:
      - travis_retry sudo add-apt-repository -y ppa:hvr/ghc
      - travis_retry sudo apt-get update
      - travis_retry sudo apt-get install --force-yes happy-1.19.4 alex-3.1.3 cabal-install-1.20
      - export PATH=/opt/alex/3.1.3/bin:/opt/happy/1.19.4/bin:/opt/cabal/1.20/bin:$PATH
      - cabal update
      - cabal install Cabal

    install:
      - cabal install --only-dependencies --enable-tests --reorder-goals

    script:
      - cabal configure --enable-tests && cabal build && cabal test
