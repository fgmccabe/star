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
    HALT = 261,
    CALL = 262,
    OCALL = 263,
    TAIL = 264,
    OTAIL = 265,
    ESCAPE = 266,
    RET = 267,
    JMP = 268,
    CASE = 269,
    DROP = 270,
    DUP = 271,
    PULL = 272,
    ROT = 273,
    RST = 274,
    BF = 275,
    BT = 276,
    CMP = 277,
    LD = 278,
    ST = 279,
    T = 280,
    A = 281,
    L = 282,
    ALLOC = 283,
    FRAME = 284,
    LOCAL = 285,
    END = 286,
    COLON = 287,
    SLASH = 288,
    DCOLON = 289,
    LBRA = 290,
    RBRA = 291,
    HASH = 292,
    NL = 293,
    DECIMAL = 294,
    STRING = 295,
    ID = 296,
    FLOAT = 297
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
 

#line 116 "/home/fgm/cafe/src/Asm/asmGrammar.h" /* yacc.c:1909  */
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
