//
// Created by Francis McCabe on 11/29/25.
//

#ifndef STAR_SSAINSTRUCTIONS_H
#define STAR_SSAINSTRUCTIONS_H

// Explicit register versions of star instructions

typeDef enum {
 sHalt,                     // Halt machine
 sAbort,                    // Abort program
 sCall,                     // Call function
 sOCall,                    // Call lambda
 sEscape,                   // Escape fun
 sXCall,                    // Call with exception
 sXOCall,                   // Call lambda with exception
 sXEscape,                  // Escape with exception
 sTCall,                    // Tail call
 sTOCall,                   // Tail into lambda
 sEntry,                    // Function preamble
 sRet,                      // Return normally
 sXRet,                     // Return with exception
 sJmp,                      // Jump to label
 sFiber,                    // Allocate fiber
 sSuspend,                  // Suspend
 sResume,
 sRetire,
 sUnderflow,
 sPhi,                      // Phi node
 sMovl,                     // Move constant value
 sMove,                     // Move variable
 sMovg,                     // Move global variable
 sSetg,                     // Set global variable
 sSav,                      // Allocate single assignment variable
 sMovs,                     // Move single assignment variable
 sSets,                     // Set single assignment variable
 sCell,                     // Allocate assignable cell
 sMovc,                     // Get contents of assignable cell
 sSetc,                     // Store into asssignable cell
 sCLbl,                     // Test for a specific label
 sCLit,                     // Test for a specific term
 sCInt,                     // Test for a specific integer value
 sCChar,                    // Test for a specific character
 sCFlt,                     // Test for a specific float
 sNth,                      // Extract nth element of term
 sStNth,                    // Store into nth element
 sICase,                    // Branch on integer
 sCase,                     // Branch on term hash
 sXCase,                    // Branch on term label index
 sIAdd,                     // Integer addition
 sISub,                     // Integer subtraction
 sIMult,                    // Integer multiply
 sIDiv,                     // Integer divide
 sIMod,                     // Integer remainder
 sIAbs,                     // Integer absolute
 sIEq,                      // Integer equal
 sILt,                      // Integer less then
 sIGe,                      // Integer greater or equal
 sCEq,                      // Character equal
 sCLt,                      // Character less then
 sCGe,                      // Character greater or equal
 sFAdd,                     // Float addition
 sFSub,                     // Float subtraction
 sFMult,                    // Float multiply
 sFDiv,                     // Float divide
 sFMod,                     // Float remainder
 sFAbs,                     // Float absolute
 sFEq,                      // Float equal
 sFLt,                      // Float less then
 sFGe,                      // Float greater or equal
 sBAnd,                     // Bitwise and
 sBOr,                      // Bitwise or
 sBXOr,                     // Bitwise exclusive or
 sBLsl,                     // Bitwise shift left
 sBLsr,                     // Bitwise logical shift right
 sBAsr,                     // Bitwise arithmetic shift right
 sBNot,                     // Bitwise negation
 sAlloc,                    // Allocate term
 sClosure,                  // Allocate closure
 sFrame,                    // Live variables
 sLine,                     // Source line
 sdBug                      // Debugging prefix
} ssaOpcode;

#endif //STAR_SSAINSTRUCTIONS_H
