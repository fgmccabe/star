	//
	// Test out bit field arm instructions
	//
	.global _start
	.align 2

_start:	mov X0, #1     // 1 = StdOut
        adr X1, helloworld // string to print
        mov X2, #13     // length of our string
        mov X16, #4     // MacOS write system call
        svc 0

// Setup the parameters to exit the program
// and then call Linux to do it.

        mov     X0, #0      // Use 0 return code
        mov     X16, #1     // Service command code 1 terminates this program
        svc     0           // Call MacOS to terminate the program

_bfc:	bfc	X12, #23, #5
	bfi	X12, X10, #23, #5
	bfxil	X12, X10, #23, #5

	bic	X5, X6, X7
	bic	X8, X9, X10, lsl #4
	bic	X8, X9, X10, lsr #4
	bic	X8, X9, X10, asr #4
	bic	X8, X9, X10, ror #4

	bics	X5, X6, X7
	bics	X8, X9, X10, lsl #4
	bics	X8, X9, X10, lsr #4
	bics	X8, X9, X10, asr #4
	bics	X8, X9, X10, ror #4

	eor	x17, x9, #0x3333333333333333
	eor	x18, x10, x12
	eor	x18, x10, x12, ror #5
	

helloworld:	.ascii "Hi there\n"
	.end
