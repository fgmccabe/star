/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Substitute the type names.  */
#define YYSTYPE         SSSTYPE
#define YYLTYPE         SSLTYPE
/* Substitute the variable and function names.  */
#define yyparse         ssparse
#define yylex           sslex
#define yyerror         sserror
#define yydebug         ssdebug
#define yynerrs         ssnerrs


/* Copy the first part of user declarations.  */
#line 5 "asmGrammar.y" /* yacc.c:339  */

#include <math.h>
#include <ooio.h>
#include "asm.h"
#include "errors.h"

#line 81 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "asmGrammar.h".  */
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
#line 29 "asmGrammar.y" /* yacc.c:355  */

  char *op;
  char *str;
  char *id;
  integer i;
  double f;
  assemInsPo ins;
  lPo lbl;
 

#line 187 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:355  */
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

/* Copy the second part of user declarations.  */
#line 39 "asmGrammar.y" /* yacc.c:358  */

  static void yyerror(YYLTYPE *loc,ioPo asmFile,pkPo *p, char const *errmsg);
  extern int sslex (YYSTYPE * asmlval_param,YYLTYPE * asmlloc_param, ioPo asmFile);

  #define locOf(asmloc)							\
    newLocation(fileName(asmInfile),asmloc.first_line,asmloc.last_line)

  static mtdPo currMtd;

  #define YYLEX_PARAM asmFile
  #define yylex sslex

#line 229 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined SSLTYPE_IS_TRIVIAL && SSLTYPE_IS_TRIVIAL \
             && defined SSSTYPE_IS_TRIVIAL && SSSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   163

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  47
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  24
/* YYNRULES -- Number of rules.  */
#define YYNRULES  68
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  151

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   301

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46
};

#if SSDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    84,    84,    86,    87,    89,    89,    91,    92,    93,
      94,    96,    96,    98,   100,   102,   103,   104,   107,   109,
     109,   111,   112,   113,   114,   115,   116,   117,   119,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     134,   135,   136,   137,   140,   141,   142,   143,   144,   145,
     146,   147,   148,   149,   152,   153,   154,   155,   158,   165,
     167,   170,   171,   172,   174,   177,   179,   180,   182
};
#endif

#if SSDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "PUBLIC", "PKG", "IMPORT", "GLOBAL",
  "HALT", "CALL", "OCALL", "TAIL", "OTAIL", "ESCAPE", "RET", "JMP", "CASE",
  "DROP", "DUP", "PULL", "ROT", "RST", "BF", "BT", "CLBL", "CMP", "LD",
  "ST", "T", "A", "L", "ALLOC", "FRAME", "LINE", "LOCAL", "END", "COLON",
  "SLASH", "AT", "DCOLON", "LBRA", "RBRA", "HASH", "NL", "DECIMAL",
  "STRING", "ID", "FLOAT", "$accept", "program", "package", "imports",
  "import", "defs", "function", "header", "instructions", "trailer", "nls",
  "instruction", "halt", "call", "literal", "load", "store", "local",
  "caseins", "heap", "directive", "label", "libName", "signature", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301
};
# endif

#define YYPACT_NINF -93

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-93)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int8 yypact[] =
{
     -37,   -93,     9,    -2,   -93,    -8,   -93,   -93,     0,    14,
      -5,    -3,    45,     6,   -93,   -13,   -93,   -37,    16,    11,
       3,   -37,    21,   -93,   100,   -93,    39,    -5,     5,    13,
      39,    39,    44,   -93,   -10,    50,   -10,    51,     4,   -93,
      43,    55,   -93,   -93,    56,    57,    58,    43,    43,    43,
      43,   -21,   -19,    66,   -10,    -5,    60,    61,   -93,    52,
     -37,   -93,   -93,   -93,   -93,   -93,   -93,   -93,    67,   -37,
      83,    39,   -37,    65,   -93,   -93,    99,   -93,   -93,   -93,
     -93,   -93,   -93,   -93,   -93,   -93,   -93,   -93,   -93,   -93,
     -93,   -93,   -93,   -93,    96,    97,    94,    99,   -93,   101,
      93,   -93,   102,   -93,   -93,   105,    -5,   -37,   -93,   -37,
      39,   -93,    39,   -37,    39,    -5,   103,   104,   106,   108,
      93,   -93,   110,    93,   109,    43,    39,    39,    39,   116,
     -93,   113,   114,   -93,   115,   -93,   117,   107,    43,   118,
     -93,   -93,   -93,   -93,   119,   -93,   -37,   121,    39,   120,
     -93
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    20,     0,     0,     1,     0,    19,     6,     0,    12,
       0,     0,     0,     0,     5,     0,    68,     0,     0,     0,
       0,     0,     0,    11,     0,     2,     4,     0,     0,     0,
       8,    18,     0,    28,     0,     0,     0,     0,     0,    34,
       0,     0,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    65,     0,
       0,    21,    22,    23,    24,    25,    26,    27,     0,     0,
       0,    10,     0,     0,    42,    41,     0,    40,    29,    30,
      32,    33,    66,    67,    31,    35,    59,    51,    52,    50,
      36,    37,    39,    38,     0,     0,     0,    47,    44,     0,
       0,    56,     0,    60,    62,     0,     0,     0,    13,     0,
      17,    61,     3,     0,     7,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,    16,    15,     9,     0,
      43,     0,     0,    53,     0,    55,     0,     0,     0,     0,
      45,    46,    54,    57,     0,    64,     0,     0,    14,     0,
      63
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -93,   -93,   -93,   -93,   -93,   -93,   -93,   -93,   -93,    80,
     -17,    85,   -93,   -93,   -24,   -93,   -93,   -92,   -93,   -93,
     -93,   -34,   -93,   -26
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     2,     7,     9,    14,    15,    23,    24,    59,    25,
       3,    60,    61,    62,    78,    63,    64,   122,    65,    66,
      67,    68,    84,    17
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      26,    69,     5,    30,    31,     1,    85,    94,    95,     4,
      99,    71,    80,    90,    91,    92,    93,    12,    96,    13,
     100,    21,    74,    75,    97,    77,   101,    98,   134,   104,
     103,   136,    22,    74,    75,    76,    77,     8,    10,    16,
       6,    11,    18,   110,    29,     1,    70,     1,    82,    83,
      19,    20,   112,   107,    27,   114,    28,    32,    72,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
     125,     6,    54,    55,    56,    57,    21,    73,    58,   129,
     126,   138,   127,    79,    81,   102,   128,    58,    86,    87,
      88,    89,   111,   115,   145,   105,   106,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,   113,   148,
      54,    55,    56,    57,   116,   117,   118,   119,   121,   108,
     120,   123,   124,   144,   109,    58,   130,   131,   133,   132,
     135,   139,   137,   140,   141,   142,   149,   143,     0,     0,
       0,   146,   147,   150
};

static const yytype_int16 yycheck[] =
{
      17,    27,     4,    20,    21,    42,    40,    28,    29,     0,
      29,    28,    36,    47,    48,    49,    50,     3,    39,     5,
      39,    34,    43,    44,    45,    46,    45,    51,   120,    55,
      54,   123,    45,    43,    44,    45,    46,    45,    38,    44,
      42,    41,    45,    60,    41,    42,    41,    42,    44,    45,
       5,    45,    69,     1,    38,    72,    45,    36,    45,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
     106,    42,    30,    31,    32,    33,    34,    43,    45,   115,
     107,   125,   109,    43,    43,    29,   113,    45,    43,    43,
      43,    43,    35,    38,   138,    45,    45,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    45,   146,
      30,    31,    32,    33,    35,    39,    39,    43,    45,    59,
      39,    39,    37,    36,    59,    45,    43,    43,    40,    43,
      40,    35,    43,    40,    40,    40,    35,    40,    -1,    -1,
      -1,    43,    43,    43
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    42,    48,    57,     0,     4,    42,    49,    45,    50,
      38,    41,     3,     5,    51,    52,    44,    70,    45,     5,
      45,    34,    45,    53,    54,    56,    57,    38,    45,    41,
      57,    57,    36,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    30,    31,    32,    33,    45,    55,
      58,    59,    60,    62,    63,    65,    66,    67,    68,    70,
      41,    57,    45,    43,    43,    44,    45,    46,    61,    43,
      61,    43,    44,    45,    69,    68,    43,    43,    43,    43,
      68,    68,    68,    68,    28,    29,    39,    45,    61,    29,
      39,    45,    29,    61,    70,    45,    45,     1,    56,    58,
      57,    35,    57,    45,    57,    38,    35,    39,    39,    43,
      39,    45,    64,    39,    37,    70,    57,    57,    57,    70,
      43,    43,    43,    40,    64,    40,    64,    43,    68,    35,
      40,    40,    40,    40,    36,    68,    43,    43,    57,    35,
      43
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    47,    48,    49,    49,    50,    50,    51,    51,    51,
      51,    52,    52,    53,    54,    55,    55,    55,    56,    57,
      57,    58,    58,    58,    58,    58,    58,    58,    59,    60,
      60,    60,    60,    60,    60,    60,    60,    60,    60,    60,
      61,    61,    61,    61,    62,    62,    62,    62,    62,    62,
      62,    62,    62,    62,    63,    63,    63,    63,    64,    65,
      66,    67,    67,    67,    67,    68,    69,    69,    70
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     5,     7,     5,     2,     0,     5,     3,     6,
       4,     2,     0,     3,     8,     3,     3,     2,     2,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       2,     2,     2,     2,     1,     2,     2,     2,     2,     2,
       1,     1,     1,     3,     2,     5,     5,     2,     1,     1,
       2,     2,     2,     4,     5,     4,     2,     5,     1,     2,
       2,     2,     2,     8,     5,     1,     1,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (&yylloc, asmFile, pkg, YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if SSDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined SSLTYPE_IS_TRIVIAL && SSLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static unsigned
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  unsigned res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
 }

#  define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, Location, asmFile, pkg); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, ioPo asmFile, pkPo *pkg)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (yylocationp);
  YYUSE (asmFile);
  YYUSE (pkg);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, ioPo asmFile, pkPo *pkg)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp, asmFile, pkg);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule, ioPo asmFile, pkPo *pkg)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                       , &(yylsp[(yyi + 1) - (yynrhs)])                       , asmFile, pkg);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, asmFile, pkg); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !SSDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !SSDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, ioPo asmFile, pkPo *pkg)
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  YYUSE (asmFile);
  YYUSE (pkg);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/*----------.
| yyparse.  |
`----------*/

int
yyparse (ioPo asmFile, pkPo *pkg)
{
/* The lookahead symbol.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

/* Location data for the lookahead symbol.  */
static YYLTYPE yyloc_default
# if defined SSLTYPE_IS_TRIVIAL && SSLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs;

    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.
       'yyls': related to locations.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

    /* The locations where the error started and ended.  */
    YYLTYPE yyerror_range[3];

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  yylsp[0] = yylloc;
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yyls1, yysize * sizeof (*yylsp),
                    &yystacksize);

        yyls = yyls1;
        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex (&yylval, &yylloc, asmFile);
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 3:
#line 86 "asmGrammar.y" /* yacc.c:1646  */
    { *pkg = newPkg((yyvsp[-5].id),(yyvsp[-3].id),(yyvsp[-1].str)); }
#line 1514 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 4:
#line 87 "asmGrammar.y" /* yacc.c:1646  */
    { *pkg = newPkg((yyvsp[-3].id),defltPkgVersion(),(yyvsp[-1].str)); }
#line 1520 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 7:
#line 91 "asmGrammar.y" /* yacc.c:1646  */
    { addImport(*pkg,(yyvsp[-3].id),(yyvsp[-1].id),False); }
#line 1526 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 8:
#line 92 "asmGrammar.y" /* yacc.c:1646  */
    { addImport(*pkg,(yyvsp[-1].id),"*",False); }
#line 1532 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 9:
#line 93 "asmGrammar.y" /* yacc.c:1646  */
    { addImport(*pkg,(yyvsp[-3].id),(yyvsp[-1].id),True); }
#line 1538 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 10:
#line 94 "asmGrammar.y" /* yacc.c:1646  */
    { addImport(*pkg,(yyvsp[-1].id),"*",True); }
#line 1544 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 14:
#line 100 "asmGrammar.y" /* yacc.c:1646  */
    { currMtd = defineMethod(*pkg,(yyvsp[-7].id),(yyvsp[-5].i),(yyvsp[-1].i),(yyvsp[-3].str)); }
#line 1550 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 18:
#line 107 "asmGrammar.y" /* yacc.c:1646  */
    { endFunction(currMtd); }
#line 1556 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 28:
#line 119 "asmGrammar.y" /* yacc.c:1646  */
    { AHalt(currMtd); }
#line 1562 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 29:
#line 121 "asmGrammar.y" /* yacc.c:1646  */
    { ACall(currMtd,(yyvsp[0].i)); }
#line 1568 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 30:
#line 122 "asmGrammar.y" /* yacc.c:1646  */
    { AOCall(currMtd,(yyvsp[0].i)); }
#line 1574 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 31:
#line 123 "asmGrammar.y" /* yacc.c:1646  */
    { AEscape(currMtd,(yyvsp[0].str)); }
#line 1580 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 32:
#line 124 "asmGrammar.y" /* yacc.c:1646  */
    { ATail(currMtd,(yyvsp[0].i)); }
#line 1586 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 33:
#line 125 "asmGrammar.y" /* yacc.c:1646  */
    { AOTail(currMtd,42); }
#line 1592 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 34:
#line 126 "asmGrammar.y" /* yacc.c:1646  */
    { ARet(currMtd); }
#line 1598 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 35:
#line 127 "asmGrammar.y" /* yacc.c:1646  */
    { AJmp(currMtd,(yyvsp[0].lbl)); }
#line 1604 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 36:
#line 128 "asmGrammar.y" /* yacc.c:1646  */
    { ABf(currMtd,(yyvsp[0].lbl)); }
#line 1610 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 37:
#line 129 "asmGrammar.y" /* yacc.c:1646  */
    {ABt(currMtd,(yyvsp[0].lbl)); }
#line 1616 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 38:
#line 130 "asmGrammar.y" /* yacc.c:1646  */
    {ACmp(currMtd,(yyvsp[0].lbl));}
#line 1622 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 39:
#line 131 "asmGrammar.y" /* yacc.c:1646  */
    { ACLbl(currMtd,(yyvsp[0].lbl)); }
#line 1628 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 40:
#line 134 "asmGrammar.y" /* yacc.c:1646  */
    { (yyval.i)=newFloatConstant(currMtd,(yyvsp[0].f)); }
#line 1634 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 41:
#line 135 "asmGrammar.y" /* yacc.c:1646  */
    { (yyval.i)=newStringConstant(currMtd,(yyvsp[0].str)); }
#line 1640 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 42:
#line 136 "asmGrammar.y" /* yacc.c:1646  */
    { (yyval.i) = newIntegerConstant(currMtd,(yyvsp[0].i)); }
#line 1646 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 43:
#line 137 "asmGrammar.y" /* yacc.c:1646  */
    { (yyval.i)=newStrctConstant(currMtd,(yyvsp[-2].id),(yyvsp[0].i)); }
#line 1652 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 44:
#line 140 "asmGrammar.y" /* yacc.c:1646  */
    { ALdC(currMtd,(yyvsp[0].i)); }
#line 1658 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 45:
#line 141 "asmGrammar.y" /* yacc.c:1646  */
    { ALdA(currMtd,(yyvsp[-1].i)); }
#line 1664 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 46:
#line 142 "asmGrammar.y" /* yacc.c:1646  */
    { ALdL(currMtd,(yyvsp[-1].i)); }
#line 1670 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 47:
#line 143 "asmGrammar.y" /* yacc.c:1646  */
    { ALdG(currMtd,(yyvsp[0].id)); }
#line 1676 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 48:
#line 144 "asmGrammar.y" /* yacc.c:1646  */
    { ADrop(currMtd); }
#line 1682 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 49:
#line 145 "asmGrammar.y" /* yacc.c:1646  */
    { ADup(currMtd); }
#line 1688 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 50:
#line 146 "asmGrammar.y" /* yacc.c:1646  */
    { ARst(currMtd,(yyvsp[0].i)); }
#line 1694 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 51:
#line 147 "asmGrammar.y" /* yacc.c:1646  */
    { APull(currMtd,(yyvsp[0].i)); }
#line 1700 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 52:
#line 148 "asmGrammar.y" /* yacc.c:1646  */
    { ARot(currMtd,(yyvsp[0].i)); }
#line 1706 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 53:
#line 149 "asmGrammar.y" /* yacc.c:1646  */
    { ANth(currMtd,(yyvsp[-1].i)); }
#line 1712 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 54:
#line 152 "asmGrammar.y" /* yacc.c:1646  */
    { AStL(currMtd,(yyvsp[-1].i)); }
#line 1718 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 55:
#line 153 "asmGrammar.y" /* yacc.c:1646  */
    { AStNth(currMtd,(yyvsp[-1].i)); }
#line 1724 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 56:
#line 154 "asmGrammar.y" /* yacc.c:1646  */
    { AStG(currMtd,(yyvsp[0].id)); }
#line 1730 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 57:
#line 155 "asmGrammar.y" /* yacc.c:1646  */
    { ATL(currMtd,(yyvsp[-1].i)); }
#line 1736 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 58:
#line 158 "asmGrammar.y" /* yacc.c:1646  */
    {
    (yyval.i)=findLocal(currMtd,(yyvsp[0].id));
    if((yyvsp[0].id)<0){
      yyerror(&yylloc,asmFile,pkg,"local var not defined");
    }
  }
#line 1747 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 59:
#line 165 "asmGrammar.y" /* yacc.c:1646  */
    { ACase(currMtd,(yyvsp[0].i)); }
#line 1753 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 60:
#line 167 "asmGrammar.y" /* yacc.c:1646  */
    { AAlloc(currMtd,(yyvsp[0].i)); }
#line 1759 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 61:
#line 170 "asmGrammar.y" /* yacc.c:1646  */
    { defineLbl(currMtd,(yyvsp[-1].lbl)); }
#line 1765 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 62:
#line 171 "asmGrammar.y" /* yacc.c:1646  */
    { AFrame(currMtd,newStringConstant(currMtd,(yyvsp[0].str))); }
#line 1771 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 63:
#line 173 "asmGrammar.y" /* yacc.c:1646  */
    { ALine(currMtd,defineLocation(currMtd,(yyvsp[-6].id),(yyvsp[-4].i),(yyvsp[-2].i),(yyvsp[0].i))); }
#line 1777 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 64:
#line 174 "asmGrammar.y" /* yacc.c:1646  */
    { defineLocal(currMtd,(yyvsp[-3].id),(yyvsp[-2].str),(yyvsp[-1].lbl),(yyvsp[0].lbl)); }
#line 1783 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 65:
#line 177 "asmGrammar.y" /* yacc.c:1646  */
    { (yyval.lbl) = newLbl(currMtd,(yyvsp[0].id)); }
#line 1789 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 66:
#line 179 "asmGrammar.y" /* yacc.c:1646  */
    { (yyval.str) = (yyvsp[0].str); }
#line 1795 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 67:
#line 180 "asmGrammar.y" /* yacc.c:1646  */
    { (yyval.str) = (yyvsp[0].id); }
#line 1801 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;

  case 68:
#line 182 "asmGrammar.y" /* yacc.c:1646  */
    { (yyval.str) = (yyvsp[0].str); if(!validSignature((yyvsp[0].str))){
  yyerror(&yylloc,asmFile,pkg,"invalid signature");
 }
 }
#line 1810 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
    break;


#line 1814 "/home/fgm/cafe/src/Asm/asmGrammar.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (&yylloc, asmFile, pkg, YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (&yylloc, asmFile, pkg, yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }

  yyerror_range[1] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc, asmFile, pkg);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  yyerror_range[1] = yylsp[1-yylen];
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, yylsp, asmFile, pkg);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, yyerror_range, 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (&yylloc, asmFile, pkg, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc, asmFile, pkg);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, yylsp, asmFile, pkg);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 187 "asmGrammar.y" /* yacc.c:1906  */


static void yyerror(YYLTYPE *loc,ioPo asmFile,pkPo *p, char const *errmsg)
{
  reportError(loc->first_line,"%s\n",errmsg);
}
