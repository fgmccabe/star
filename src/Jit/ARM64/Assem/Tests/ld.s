	//
	// Test out arm ld instructions
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

	;; ldaddab	w3, w6, [x4]
	;; ldaddalb	w3, w6, [x4]
	;; ldaddb	w3, w6, [x4]
	;; ldaddlb	w3, w6, [x4]
	
	;; ldaddah	w3, w6, [x4]
	;; ldaddalh	w3, w6, [x4]
	;; ldaddh	w3, w6, [x4]
	;; ldaddlh	w3, w6, [x4]
	
	;; ldadd	x3, x6, [x4]
	;; ldadda	x3, x6, [x4]
	;; ldaddal	x3, x6, [x4]
	;; ldaddl	x3, x6, [x4]

	;; ldp	X10, X11, [X3], #8
	;; ldp	X10, X8, [X5, #-8]!
	;; ldp	X15, X20, [X20, #16]

	;; ldpsw	X10, X11, [X3], #8
	;; ldpsw	X10, X8, [X5, #-8]!
	;; ldpsw	X15, X20, [X20, #16]

	;; ldr	X10, [X3], #8
	;; ldr	X10, [X5, #-8]!
	;; ldr	X15, [X20, #16]

	;; ldr	X19, helloworld
	;; ldr	X21, [X22, X7, SXTX #3]

	;; ldrb	w19, [X3], #8
	;; ldrb	w10, [X5, #-8]!
	;; ldrb	w15, [X20, #16]
	;; ldrb    w21, [X22, x7, sxtx]
	
	;; ldrh	w19, [X3], #8
	;; ldrh	w10, [X5, #-8]!
	;; ldrh	w15, [X20, #16]
	;; ldrh    w21, [X22, x7, sxtx]
	
	;; ldrsb	w19, [X3], #8
	;; ldrsb	w10, [X5, #-8]!
	;; ldrsb	w15, [X20, #16]
	;; ldrsb    w21, [X22, x7, sxtx]
	
	;; ldrsh	w19, [X3], #8
	;; ldrsh	w10, [X5, #-8]!
	;; ldrsh	w15, [X20, #16]
	;; ldrsh    w21, [X22, x7, sxtx]

	;; ldrsw	X10, [X3], #8
	;; ldrsw	X10, [X5, #-8]!
	;; ldrsw	X15, [X20, #16]

	;; ldrsw	X19, helloworld
	;; ldrsw	X21, [X22, X7, SXTX #2]

	;; ldur	X29, [X3, #-8]
	;; ldurb	w29, [X3, #-8]
	;; ldurh	w29, [X3, #-8]
	;; ldursb	X29, [X3, #-8]
	;; ldursh	X29, [X3, #-8]
	;; ldursw	X29, [X3, #-8]

	;; ldtr	X0, [X1, #-8]
	ldr	X15, [X20, #-16]
	ldtr	X15, [X20, #-16]

	ldxp	X12, x16, [x2]
	ldxr	X13, [X9]
	ldxrb	w13, [X9]
	ldxrh	w13, [X9]
	

helloworld:	.ascii "Hi there\n"
	.end
