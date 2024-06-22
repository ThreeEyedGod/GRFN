all: build test doc profile package
	
build:
	@echo "Build"
	@rm -rf ./build/*
	@rm -rf *.tix
	@rm -rf hpc*.html
	cabal clean
	cabal update
	cabal build

test:
	@echo "Testing"
	cabal test 

doc:
	@echo "creating documentation"
	--cabal v2-haddock --haddock-for-hackage --haddock-hyperlink-source --haddock-quickjump --enable-doc
	--cabal haddock --haddock-executables --haddock-for-hackage --haddock-hyperlink-source --haddock-quickjump --enable-doc  
	cabal haddock --haddock-for-hackage --haddock-hyperlink-source --haddock-quickjump --enable-doc  

benchmark:
	@echo "creating benchmarks"
	cabal bench --benchmark-options='--timeout 10000000' --benchmark-options='+RTS -I0 -A16m -N3 -H24m'
	hpc report grfn-test 
	hpc markup grfn-test

profile:
	@echo "creating profile"
	@rm -rf *.tix
	cabal run grfn-exe -- +RTS -p -s -N4

main:
	@echo "creating main"
	@rm -rf *.tix
	cabal run grfn-exe -- +RTS -N4

profile_stack:
	@echo "creating profile w/stack"
	stack build --profile
	stack exec -- grfn-exe  100000 +RTS -p -N4 -RTS

package:
	@echo "creating tarball"
	cabal sdist

upload:
	@echo "uploading docs hack"
	rm -rf ./dist-docs/*
	cabal haddock --builddir=dist-docs --haddock-for-hackage --haddock-option=--hyperlinked-source
	cabal upload --publish -d dist-docs/*-docs.tar.gz