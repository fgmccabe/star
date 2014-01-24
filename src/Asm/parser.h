/* A Bison parser, made by GNU Bison 3.0.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

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

#ifndef YY_SS_PARSER_H_INCLUDED
# define YY_SS_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int ssdebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    HALT = 258,
    CALL = 259,
    TAIL = 260,
    ENTER = 261,
    ESCAPE = 262,
    RET = 263,
    JMP = 264,
    CASE = 265,
    LD = 266,
    ST = 267,
    POP = 268,
    SWAP = 269,
    DUP = 270,
    CAS = 271,
    A = 272,
    L = 273,
    E = 274,
    ALLOC = 275,
    I2F = 276,
    F2I = 277,
    ADD = 278,
    ADDF = 279,
    SUB = 280,
    SUBF = 281,
    MUL = 282,
    MULF = 283,
    DIV = 284,
    DIVF = 285,
    REM = 286,
    INC = 287,
    DEC = 288,
    LEFT = 289,
    RIGHT = 290,
    ASR = 291,
    CMP = 292,
    CMPF = 293,
    BZ = 294,
    BNZ = 295,
    BLE = 296,
    BLT = 297,
    BGE = 298,
    BGT = 299,
    FRAME = 300,
    LOCAL = 301,
    END = 302,
    COLON = 303,
    DCOLON = 304,
    LBRA = 305,
    RBRA = 306,
    NL = 307,
    CHAR = 308,
    DECIMAL = 309,
    STRING = 310,
    ID = 311,
    FLOAT = 312
  };
#endif
/* Tokens.  */
#define HALT 258
#define CALL 259
#define TAIL 260
#define ENTER 261
#define ESCAPE 262
#define RET 263
#define JMP 264
#define CASE 265
#define LD 266
#define ST 267
#define POP 268
#define SWAP 269
#define DUP 270
#define CAS 271
#define A 272
#define L 273
#define E 274
#define ALLOC 275
#define I2F 276
#define F2I 277
#define ADD 278
#define ADDF 279
#define SUB 280
#define SUBF 281
#define MUL 282
#define MULF 283
#define DIV 284
#define DIVF 285
#define REM 286
#define INC 287
#define DEC 288
#define LEFT 289
#define RIGHT 290
#define ASR 291
#define CMP 292
#define CMPF 293
#define BZ 294
#define BNZ 295
#define BLE 296
#define BLT 297
#define BGE 298
#define BGT 299
#define FRAME 300
#define LOCAL 301
#define END 302
#define COLON 303
#define DCOLON 304
#define LBRA 305
#define RBRA 306
#define NL 307
#define CHAR 308
#define DECIMAL 309
#define STRING 310
#define ID 311
#define FLOAT 312

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 26 "parser.y" /* yacc.c:1915  */

  uniChar ch;
  uniChar *op;
  uniChar *str;
  uniChar *id;
  integer i;
  double f;
  assemInsPo ins;
  lPo lbl;
 

#line 180 "parser.h" /* yacc.c:1915  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif



int ssparse (ioPo yyInfile, pkgPo pkg);

#endif /* !YY_SS_PARSER_H_INCLUDED  */
