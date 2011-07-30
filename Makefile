shpider:
	cabal install --prefix=$$HOME --user --ghc-options="-hide-package monads-tf"

all: shpider
	ghc -O2 --make shpidertest.hs -hide-package monads-tf

clean:
	-rm shpidertest