all: do_build do_test do_gendoc do_benchmark db_profile
	
do_build:
	@echo "Build"
	@rm -rf ./build/*
	@rm -rf *.tix
	@rm -rf hpc*.html
	cabal clean
	cabal build

do_test:
	@echo "Testing"
	cabal test 

do_gendoc:
	@echo "creating haddock docs"
	cabal haddock

do_benchmark:
	@echo "creating benchmarks"
	cabal bench --benchmark-options='--timeout 10000000' --benchmark-options="+RTS -T"
	hpc report grfn-test 
	hpc markup grfn-test

db_profile:
	@echo "creating profile"
	rm -rf *.tix
	stack build --profile
	stack exec -- /Users/mandeburung/Documents/GRFN/.stack-work/install/aarch64-osx/83b8e52cf51e0636cb104a94336fdd5c4077f15748a21c6b60971868afacbdc5/9.6.5/bin/grfn-exe  100000 +RTS -p -RTS
	