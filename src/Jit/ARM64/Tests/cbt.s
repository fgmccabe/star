	//
	// Test out bit arm instructions
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

	cls	X11,X9
	clz	X9,X7

	cmn	X21, X9, uxtx #3
	cmn	X20, #0xff
	cmn	X20, #0xff0, lsl #12
	cmn	X19, X5, lsl #5
	cmn	X19, X5, asr #5

	cmp	X21, X9, uxtx #3
	cmp	X20, #0xff
	cmp	X20, #0xff0, lsl #12
	cmp	X19, X5, lsl #5
	cmp	X19, X5, asr #5

	cinc	X28, X20, eq
	cinv	X28, X20, cc
	cneg	X28, X20, hi

	csel	X17, X13, X20, le
	cset	X16, le
	csetm	X16, le

	csinc	X14, X13, X11, gt
	csinv	X14, X13, X11, gt
	csneg	X14, X13, X11, gt
	

helloworld:	.ascii "Hi there\n"
	.end
