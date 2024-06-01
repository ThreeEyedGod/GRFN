all: do_build do_test do_gendoc
allplusbench : all do_benchmark
	
do_build:
	@echo "Build"
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
	cabal bench --benchmark-options='--timeout 100000000' --benchmark-options="+RTS -T"
