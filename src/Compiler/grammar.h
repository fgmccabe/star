/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     COMMA = 258,
     SEMI = 259,
     PRCENT = 260,
     COLON = 261,
     DCOLON = 262,
     ASSIGN = 263,
     ARROW = 264,
     THINARROW = 265,
     EQUAL = 266,
     LPAR = 267,
     RPAR = 268,
     LBRCE = 269,
     RBRCE = 270,
     ANON = 271,
     PROCEDURE = 272,
     FUNCTION = 273,
     PATTERN = 274,
     MEMO = 275,
     TYPE = 276,
     OR = 277,
     LET = 278,
     SWITCH = 279,
     DEFAULT = 280,
     IN = 281,
     VALOF = 282,
     VALIS = 283,
     IMPORT = 284,
     VAR = 285,
     LEAVE = 286,
     IS = 287,
     NOTHING = 288,
     DO = 289,
     WHILE = 290,
     LOOP = 291,
     GOTO = 292,
     IF = 293,
     THEN = 294,
     ELSE = 295,
     SYNC = 296,
     TRY = 297,
     CATCH = 298,
     THROW = 299,
     MATCHES = 300,
     ASSERT = 301,
     LESS = 302,
     LEQ = 303,
     GT = 304,
     GEQ = 305,
     NOTEQUAL = 306,
     PLUS = 307,
     MINUS = 308,
     TIMES = 309,
     DIVIDE = 310,
     LSHIFT = 311,
     RSHIFT = 312,
     UMINUS = 313,
     BITAND = 314,
     BITOR = 315,
     BITXOR = 316,
     BITNEG = 317,
     DECIMAL = 318,
     LONG = 319,
     STRING = 320,
     ID = 321,
     FLOAT = 322
   };
#endif
/* Tokens.  */
#define COMMA 258
#define SEMI 259
#define PRCENT 260
#define COLON 261
#define DCOLON 262
#define ASSIGN 263
#define ARROW 264
#define THINARROW 265
#define EQUAL 266
#define LPAR 267
#define RPAR 268
#define LBRCE 269
#define RBRCE 270
#define ANON 271
#define PROCEDURE 272
#define FUNCTION 273
#define PATTERN 274
#define MEMO 275
#define TYPE 276
#define OR 277
#define LET 278
#define SWITCH 279
#define DEFAULT 280
#define IN 281
#define VALOF 282
#define VALIS 283
#define IMPORT 284
#define VAR 285
#define LEAVE 286
#define IS 287
#define NOTHING 288
#define DO 289
#define WHILE 290
#define LOOP 291
#define GOTO 292
#define IF 293
#define THEN 294
#define ELSE 295
#define SYNC 296
#define TRY 297
#define CATCH 298
#define THROW 299
#define MATCHES 300
#define ASSERT 301
#define LESS 302
#define LEQ 303
#define GT 304
#define GEQ 305
#define NOTEQUAL 306
#define PLUS 307
#define MINUS 308
#define TIMES 309
#define DIVIDE 310
#define LSHIFT 311
#define RSHIFT 312
#define UMINUS 313
#define BITAND 314
#define BITOR 315
#define BITXOR 316
#define BITNEG 317
#define DECIMAL 318
#define LONG 319
#define STRING 320
#define ID 321
#define FLOAT 322




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 30 "grammar.y"
{
  uniChar *op;
  uniChar *str;
  uniChar *id;
  uniChar ch;
  integer i;
  double f;
  sxPo a;
  lxPo s;
 }
/* Line 1529 of yacc.c.  */
#line 194 "grammar.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


