
genprolog:
	stack build && stack exec -- simple-prover-pl-exe "testfiles/prover.thy" >| testfiles/prover-generated.pl

testprolog: genprolog
	swipl -s testfiles/testsuite.pl

