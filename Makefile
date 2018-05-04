all:
	jbuilder build cmm_of_wasm.exe
	cp _build/default/cmm_of_wasm.exe cmm_of_wasm

clean:
	jbuilder clean
