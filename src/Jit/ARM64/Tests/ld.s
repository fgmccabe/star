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

	ldaddab	w3, w6, [x4]
	ldaddalb	w3, w6, [x4]
	ldaddb	w3, w6, [x4]
	ldaddlb	w3, w6, [x4]
	
	ldaddah	w3, w6, [x4]
	ldaddalh	w3, w6, [x4]
	ldaddh	w3, w6, [x4]
	ldaddlh	w3, w6, [x4]
	
	ldadd	x3, x6, [x4]
	ldadda	x3, x6, [x4]
	ldaddal	x3, x6, [x4]
	ldaddl	x3, x6, [x4]

	ldp	X10, X11, [X3], #8
	ldp	X10, X8, [X5, #-8]!
	ldp	X15, X20, [X20, #16]

	ldpsw	X10, X11, [X3], #8
	ldpsw	X10, X8, [X5, #-8]!
	ldpsw	X15, X20, [X20, #16]

	ldr	X10, [X3], #8
	ldr	X10, [X5, #-8]!
	ldr	X15, [X20, #16]

	ldr	X19, helloworld
	ldr	X21, [X22, X7, SXTX #3]
	
	
helloworld:	.ascii "Hi there\n"
	.end
