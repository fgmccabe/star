	//
	// Test out saving registers
	//
	.global _start
	.align 2

_start:	mov X0, #1     // 1 = StdOut
        adr X1, helloworld // string to print
        mov X2, #13     // length of our string
        mov X16, #4     // MacOS write system call
        svc 0

        stp X20, XZR, [SP, #-16]!
        stp X10, X17, [SP, #-16]!
        stp X4, X5, [SP, #-16]!
        stp X0, X3, [SP, #-16]!

        ldp XZR, X20, [SP], #16
        ldp X17, X10, [SP], #-16
        ldp X5, X4, [SP], #-16
        ldp X3, X0, [SP], #-16
	
	
// Setup the parameters to exit the program
// and then call Linux to do it.

        mov     X0, #0      // Use 0 return code
        mov     X16, #1     // Service command code 1 terminates this program
        svc     0           // Call MacOS to terminate the program

helloworld:	.ascii "Hi there\n"
	.end
