all:
	jbuilder build src/bin/cmm_of_wasm.exe
	cp -r src/rts includes/
	cp _build/default/src/bin/cmm_of_wasm.exe cmm_of_wasm

clean:
	jbuilder clean
