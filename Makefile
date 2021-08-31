install:
	cabal haddock --haddock-all
	cabal install --overwrite-policy=always
	cp matelcli.1 /usr/local/man/man1/
uninstall:
	rm /usr/local/man/man1/matelcli.1
