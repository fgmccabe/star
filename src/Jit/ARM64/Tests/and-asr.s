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

_adc:	and	X12, X13, #0xf0
	;; adc	X1, X2, X3
	;; adcs	X12, x13,X14
	;; adcs	X1, X2, X3

	;; add     X3, X4, X6
	;; add     X3, X5, 135
	;; add     X3, X5, 135, lsl 12
	;; add     X2, X4, X6, lsl 3
	;; add     X1, X12, X18, lsr 3
	;; add     X1, X12, X18, asr 3

	;; adds     X3, X4, X6
	;; adds     X3, X5, 135
	;; adds     X3, X5, 135, lsl 12
	;; adds     X2, X4, X6, lsl 3
	;; adds     X1, X12, X18, lsr 3
	;; adds     X1, X12, X18, asr 3

helloworld:	.ascii "Hi there\n"
	.end
