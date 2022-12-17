adc-add: adc-add.o
	ld -o adc-add adc-add.o \
         -lSystem \
	-syslibroot `xcrun -sdk macosx --show-sdk-path` \
	-e _start \
	-arch arm64

adr: adr.o
	ld -o adr adr.o \
         -lSystem \
	-syslibroot `xcrun -sdk macosx --show-sdk-path` \
	-e _start \
	-arch arm64

and-asr: and-asr.o
	ld -o and-asr and-asr.o \
         -lSystem \
	-syslibroot `xcrun -sdk macosx --show-sdk-path` \
	-e _start \
	-arch arm64

b: b.o
	ld -o b b.o \
         -lSystem \
	-syslibroot `xcrun -sdk macosx --show-sdk-path` \
	-e _start \
	-arch arm64

bfc: bf.o
	ld -o bfc bf.o \
         -lSystem \
	-syslibroot `xcrun -sdk macosx --show-sdk-path` \
	-e _start \
	-arch arm64

cas: cas.o
	ld -o cas cas.o \
         -lSystem \
	-syslibroot `xcrun -sdk macosx --show-sdk-path` \
	-e _start \
	-arch arm64

cbt: cbt.o
	ld -o cbt cbt.o \
         -lSystem \
	-syslibroot `xcrun -sdk macosx --show-sdk-path` \
	-e _start \
	-arch arm64

.o: .s
	as -o $@.o $@.s
