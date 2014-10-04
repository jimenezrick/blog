.PHONY: all clean

all: .cabal-sandbox/bin/rlog

.cabal-sandbox/bin/rlog: Main.hs | .cabal-sandbox
	cabal install

.cabal-sandbox:
	cabal sandbox init
	cabal install --only-dependencies

clean:
	cabal sandbox delete
	cabal clean
