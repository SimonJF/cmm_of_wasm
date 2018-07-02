all:
	jbuilder build src/bin/cmm_of_wasm.exe
	cp -r src/rts includes/
	cp _build/default/src/bin/cmm_of_wasm.exe cmm_of_wasm

ounit_tests:
	jbuilder build test/ounit_tests/test.exe
	cp _build/default/test/ounit_tests/test.exe test/test

clean:
	jbuilder clean
