/*
  Parse the UnicodeData.txt file, building a table of 
  character atributes as a C module
  (c) 1999 and beyond F.G.McCabe

  Contact: Francis McCabe <fmccabe@gmail.com>
 */

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define MAXCODE (1u<<20u)

typedef enum {
  Cc, Cf, Cn, Co, Cs,
  Ll, Lm, Lo, Lt, Lu,
  Mc, Me, Mn,
  Nd, Nl, No,
  Pc, Pd, Pe, Pf, Pi, Po, Ps,
  Sc, Sk, Sm, So,
  Zl, Zp, Zs, Other
} CharCatagory;

struct {
  char cat[8];
  int digitVal;
} Uni[MAXCODE];

int main(int argc, char **argv) {
  if (argc < 3) {
    printf("usage %s: <unicode.txt> <uniChar.c>\n", argv[0]);
    exit(1);
  } else {
    FILE *in = fopen(argv[1], "r");
    FILE *out = fopen(argv[2], "w");
    unsigned long first = 0;
    unsigned long last = 0;
    char cat[16];
    int digitVal = -1;

    {
      int i;
      for (i = 0; i < MAXCODE; i++)
        strcpy(Uni[i].cat, "Other");
    }

    while (!feof(in)) {
      unsigned long code = -1;
      char name[1024];

      size_t len = 0;
      char *line = fgetln(in, &len);

      if (line != NULL) {
        char *p = line;
        code = strtoul(p, &p, 16);                /* extract the code value */
        assert(*p == ';');

        {
          char *n = name;

          while (*++p != ';')
            *n++ = *p;
          *n = '\0';
        }

        if (strcmp(name + strlen(name) - strlen("First>"), "First>") == 0)
          first = code;
        else if (strcmp(name + strlen(name) - strlen("Last>"), "Last>") == 0)
          last = code;

        p++;                                    /* skip over the character name */

        cat[0] = *p++;                            /* general catagory */
        cat[1] = *p++;
        cat[2] = '\0';
        p = strchr(p + 1, ';');                    /* skip over combining class */
        p = strchr(p + 1, ';');                    /* skip over bidirectional catagory */
        p = strchr(p + 1, ';');                    /* skip over char decomp mapping */
        digitVal = atoi(p + 1);                   /* digit value */
      }

      if (first == 0 && last == 0) {
        if (code != -1 && code < MAXCODE) {
          strcpy(Uni[code].cat, cat);
          Uni[code].digitVal = digitVal;
        }
      } else if (first != 0 && last != 0 && first != -1 && last < MAXCODE) {
        unsigned long ii;
        for (ii = first; ii <= last; ii++) {
          strcpy(Uni[ii].cat, cat);
          Uni[ii].digitVal = digitVal;
        }
        first = last = 0;
      }
    }

    /* Generate the basic general catagories table */
    fprintf(out, "/* General catagories table */\n");
    fprintf(out, "/* WARNING: Automatically generated, DO NOT EDIT */\n\n");
    fprintf(out, "#include \"unistrP.h\"\n\n");
    fprintf(out, "UniCharCategory genCatTbl[] = {\n");

    {
      long i;
      char sep[32] = "/* 0x00 */  ";

      for (i = 0; i < MAXCODE; i++) {
        fprintf(out, "%s%s", sep, Uni[i].cat);
        if (i % 32 == 31)
          sprintf(sep, ",\n/* 0x%lx */  ", i + 1);
        else
          sprintf(sep, ",");
      }

      fprintf(out, "};\n");
    }

    fprintf(out, "short digitValTbl[] = {\n");

    {
      long i;
      char sep[32] = "/* 0x00 */  ";

      for (i = 0; i < MAXCODE; i++) {
        fprintf(out, "%s%d", sep, Uni[i].digitVal);
        if (i % 32 == 31)
          sprintf(sep, ",\n/* 0x%lx */  ", i + 1);
        else
          sprintf(sep, ",");
      }

      fprintf(out, "};\n");
    }
    fclose(out);
  }

  return 0;
}
