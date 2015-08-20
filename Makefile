all : build

build: .cabal-sandbox _depends
	cabal install

sandbox : .cabal-sandbox

.cabal-sandbox :
	cabal sandbox init

_depends : .cabal-sandbox dbr.cabal
	cabal install --only-dependencies --enable-tests --haddock-all
	cabal configure --enable-tests
	touch _depends

clean:
	cabal clean
	rm -rf dist

clean-all: clean
	find -name '*.hi' -exec rm {} \;
	find -name '*.o' -exec rm {} \;
	cabal sandbox delete

sdist :
	cabal sdist

test : _depends
	cabal install
	cabal test --show-details=always
		
haddock : _depends
	cabal haddock --html
	@echo "file://$$PWD/dist/doc/html/dbr/index.html"

hlint :
	hlint --hint support/HLint.hs src
