#ifndef STAR_OPTIONS_H_
#define STAR_OPTIONS_H_

#ifndef MAX_SYMB_LEN
#define MAX_SYMB_LEN 1024
#endif

#define EXIT_SUCCEED 0    /* Normal exit */
#define EXIT_FAIL 1        /* Failing exit */
#define EXIT_ERROR 2      // Error exit

#define STAR_LOGFILE "STAR_LOGFILE"
#define STAR_REPO "STAR_REPO"
#define STAR_HOME "STAR_HOME"
#define STAR_WD "STAR_WD"
#define STAR_ROOT_WD "STAR_ROOT_WD"
#define STAR_INIT_HEAP "STAR_INITIAL_HEAP"
#define STAR_MAX_HEAP "STAR_MAX_HEAP"
#define STAR_MIN_STACK "STAR_MIN_STACK"
#define STAR_DFLT_STACK "STAR_DFLT_STACK"
#define STAR_STACK_REGION "STAR_STACK_REGION"
#define STAR_VERIFY "STAR_ENABLE_VERIFY"
#define STAR_MAX_LABELS "STAR_MAX_LABELS"

typedef enum{
  noTracing,
  generalTracing,
  detailedTracing
} tracingLevel;

#endif
