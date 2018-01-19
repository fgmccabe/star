#include <stdio.h>
#include <stdlib.h>

// Utility program to generate the opcodes.h header file

#undef instruction
#define instruction(mnem,A1,cmnt) fprintf(out,"  "#mnem",\n");

#define lastInstruction

int main(int argc, char **argv) {
  FILE *out = stdout;

  if (argc >= 2)
    out = fopen(argv[1], "w");

  fprintf(out, "/* Automatically generated, do not edit */\n");
  fprintf(out,"#ifndef _CAFE_OPCODE_H_\n");
  fprintf(out,"#define _CAFE_OPCODE_H_\n");
  fprintf(out, "typedef enum {\n");
#include "instructions.h" /* Pick up the instructions specification */
  fprintf(out, "  label,\n");
  fprintf(out, "  illegalOp\n");
  fprintf(out, "} OpCode;\n\n");
  fprintf(out,"#endif //_CAFE_OPCODE_H_\n");
#undef instruction

  fclose(out);
  exit(0);
}
