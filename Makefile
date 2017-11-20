
genprolog:
	stack build && stack exec -- simple-prover-pl-exe "testfiles/prover.thy" >| testfiles/prover-generated.pl

testprolog: genprolog
	swipl -q -f testfiles/testsuite.pl

