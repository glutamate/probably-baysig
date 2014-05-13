cibuild:
	cabal sandbox init
	cabal install --force-reinstalls

citest: