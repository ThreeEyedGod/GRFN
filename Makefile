all: do_build do_test do_gendoc do_profile
	
do_build:
	@echo "Build"
	@rm -rf ./build/*
	@rm -rf *.tix
	@rm -rf hpc*.html
	cabal clean
	cabal update
	cabal build

do_test:
	@echo "Testing"
	cabal test 

do_gendoc:
	@echo "creating haddock docs"
	cabal haddock

do_benchmark:
	@echo "creating benchmarks"
	cabal bench --benchmark-options='--timeout 10000000' --benchmark-options='+RTS -I0 -A16m -N3 -H24m'
	hpc report grfn-test 
	hpc markup grfn-test

do_profile:
	@echo "creating profile"
	@rm -rf *.tix
	cabal run grfn-exe -- +RTS -p -N4

do_main:
	@echo "creating main"
	@rm -rf *.tix
	cabal run grfn-exe -- +RTS -N4

do_profile_stack:
	@echo "creating profile w/stack"
	stack build --profile
	stack exec -- grfn-exe  100000 +RTS -p -N4 -RTS
