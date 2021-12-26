install:
	# \| Documentation rocks.
	cabal haddock --haddock-all
	# \| Keeping the old versions just seems goofy.  New versions of
	# Matel _generally_ bring only improvements.
	cabal install --overwrite-policy=always
	cp matelcli.1 matel.1 /usr/local/man/man1/
uninstall:
	for i in matelcli matel
	do
		rm /usr/local/man/man1/$i.1
	done
