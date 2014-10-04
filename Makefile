.PHONY: clean

# TODO: bin target, flag static, strip install?
all: | .cabal-sandbox
	cabal configure
	cabal build
	#cabal install

.cabal-sandbox:
	cabal sandbox init
	cabal install --only-dependencies

clean:
	cabal sandbox delete
	cabal clean
