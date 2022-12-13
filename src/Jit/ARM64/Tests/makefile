adc-add: adc-add.o
	ld -o adc-add adc-add.o \
         -lSystem \
	-syslibroot `xcrun -sdk macosx --show-sdk-path` \
	-e _start \
	-arch arm64

.o: .s
	as -o $@.o $@.s
