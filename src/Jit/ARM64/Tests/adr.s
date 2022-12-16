	//
	// Test out adr arm instructions
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

_adc:	adr	X10, _adc
	adr	x12, 1f
1:	b.eq	1b
	b.nv    1b
	b.hi    2f
2:	

helloworld:	.ascii "Hi there\n"
	.end
