/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_SS_HOME_FGM_CAFE_SRC_ASM_ASMGRAMMAR_H_INCLUDED
# define YY_SS_HOME_FGM_CAFE_SRC_ASM_ASMGRAMMAR_H_INCLUDED
/* Debug traces.  */
#ifndef SSDEBUG
# if defined YYDEBUG
#if YYDEBUG
#   define SSDEBUG 1
#  else
#   define SSDEBUG 0
#  endif
# else /* ! defined YYDEBUG */
#  define SSDEBUG 1
# endif /* ! defined YYDEBUG */
#endif  /* ! defined SSDEBUG */
#if SSDEBUG
extern int ssdebug;
#endif

/* Token type.  */
#ifndef SSTOKENTYPE
# define SSTOKENTYPE
  enum sstokentype
  {
    PUBLIC = 258,
    PKG = 259,
    IMPORT = 260,
    GLOBAL = 261,
    HALT = 262,
    CALL = 263,
    OCALL = 264,
    TAIL = 265,
    OTAIL = 266,
    ESCAPE = 267,
    RET = 268,
    JMP = 269,
    CASE = 270,
    DROP = 271,
    DUP = 272,
    PULL = 273,
    ROT = 274,
    RST = 275,
    BF = 276,
    BT = 277,
    CLBL = 278,
    CMP = 279,
    LD = 280,
    ST = 281,
    T = 282,
    A = 283,
    L = 284,
    ALLOC = 285,
    FRAME = 286,
    LINE = 287,
    LOCAL = 288,
    END = 289,
    COLON = 290,
    SLASH = 291,
    AT = 292,
    DCOLON = 293,
    LBRA = 294,
    RBRA = 295,
    HASH = 296,
    NL = 297,
    DECIMAL = 298,
    STRING = 299,
    ID = 300,
    FLOAT = 301
  };
#endif

/* Value type.  */
#if ! defined SSSTYPE && ! defined SSSTYPE_IS_DECLARED

union SSSTYPE
{
#line 29 "asmGrammar.y" /* yacc.c:1909  */

  char *op;
  char *str;
  char *id;
  integer i;
  double f;
  assemInsPo ins;
  lPo lbl;
 

#line 120 "/home/fgm/cafe/src/Asm/asmGrammar.h" /* yacc.c:1909  */
};

typedef union SSSTYPE SSSTYPE;
# define SSSTYPE_IS_TRIVIAL 1
# define SSSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined SSLTYPE && ! defined SSLTYPE_IS_DECLARED
typedef struct SSLTYPE SSLTYPE;
struct SSLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define SSLTYPE_IS_DECLARED 1
# define SSLTYPE_IS_TRIVIAL 1
#endif



int ssparse (ioPo asmFile, pkPo *pkg);

#endif /* !YY_SS_HOME_FGM_CAFE_SRC_ASM_ASMGRAMMAR_H_INCLUDED  */
