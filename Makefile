
genprolog:
	stack build && stack exec -- simple-prover-pl-exe "testfiles/prover.thy" >| testfiles/prover-generated.pl

testprolog: genprolog
	swipl -s testfiles/testsuite.pl

testprolog-persist: genprolog
	swipl -s testfiles/testsuite.pl -- persist
