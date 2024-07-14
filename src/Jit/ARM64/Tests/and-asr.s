	//
	// Test out and - asr arm instructions
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

	;; and	X12, X13, #0x3333333333333333
	;; and	X12, X13, #0xff00
	;; and     X1, X2, X3
	;; and	X3, X5, X6, lsl 3
	;; and	X3, X5, X6, lsr 3
	;; and	X3, X5, X6, ror 3

	;; ands	X12, X13, #0x3333333333333333
	;; ands	X12, X13, #0xff00
	;; ands     X1, X2, X3
	;; ands	X3, X5, X6, lsl 3
	;; ands	X3, X5, X6, lsr 3
	;; ands	X3, X5, X6, ror 3

	;; asr	 X12, X13, #0xf
	;; asr     X1, X2, X3

	tst	X13, #0x3333333333333333
	tst	X13, #0xff00
	tst     X2, X3
	tst	X5, X6, lsl 3
	tst	X5, X6, lsr 3
	tst	X5, X6, ror 3
	
helloworld:	.ascii "Hi there\n"
	.end
