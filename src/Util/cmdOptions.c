//
// Created by Francis McCabe on 6/29/17.
//

#include <memory.h>
#include <stdlib.h>
#include <cmdOptions.h>
#include <ooio.h>

void splitFirstArg(int argc, char **argv, int *newArgc, char ***newArgv) {
  /*
     Splits the first command-line argument into multiple
     arguments if it starts with "-%". The
     delimiter is the first character following the percent sign.
     For example: "-%%-pdir1%-pdir2" will be split into
     two arguments "-pdir1", "-pdir2".

     This helps to work around the limitation that #! scripts
     can pass a maximum of one argument to the interpreter
     under certain operating systems (eg. Linux).
  */

  *newArgc = argc;
  *newArgv = argv;

  if (argc < 2)
    return;

  if (uniNCmp(argv[1], uniStrLen(argv[1]), "-%", 2) == same) {
    char delimiter = argv[1][2];
    int extra = 0, arg = 1;
    char *p;

    /* Count number of & in passed arguments */
    p = argv[1] + 3;
    do {
      char *q = strchr(p, delimiter);
      if (q == NULL)
        break;

      p = q + 1;
      extra++;
    } while (*p != '\0');


    /* We didn't find any delimiters */
    if (extra == 0)
      return;

    /* Make the extra arguments */
    *newArgc = argc + extra;
    *newArgv = (char **) malloc(*newArgc * sizeof(char *));
    (*newArgv)[0] = argv[0];

    p = argv[1] + 3;
    do {
      char *q = strchr(p, delimiter);
      if (q == NULL) {
        (*newArgv)[arg++] = p;
        break;
      } else {
        size_t len = (size_t) (q - p);
        char *data = (char *) malloc(len + 1);

        strncpy(data, p, len);
        data[len] = '\0';
        (*newArgv)[arg++] = data;
        p = q + 1;
      }
    } while (True);
  }
}

int processOptions(char *copyRight, int argc, char **argv, Option *options, int optionCount) {
  int ix;

  logical processedOptions[optionCount];

  for (ix = 0; ix < optionCount; ix++)
    processedOptions[ix] = False;

  for (ix = 1; ix < argc; ix++) {
    char *opt = argv[ix];

    if (uniIsLit(opt, "--"))
      break;
    else if (uniIsLitPrefix(opt, "-") && uniStrLen(opt) > 1) {
      char shortOpt = opt[1];
      for (int j = 0; j < optionCount; j++) {
        if (options[j].shortName == shortOpt) {
          switch (options[j].hasArg) {
            case hasArgument: {
              if (uniStrLen(opt) == 2) {
                if (ix < argc - 1) {
                  if (options[j].setter(argv[++ix], True, options[j].cl) != Ok)
                    goto failOptions;
                  else {
                    processedOptions[j] = True;
                    goto optionLoop;
                  }
                } else
                  goto failOptions;
              } else {
                if (options[j].setter(opt + 2, True, options[j].cl) != Ok)
                  goto failOptions;
                else {
                  processedOptions[j] = True;
                  goto optionLoop;
                }
              }
            }
            case noArgument: {
              if (uniStrLen(opt) == 2) {
                if (options[j].setter(NULL, True, options[j].cl) != Ok)
                  goto failOptions;
                else {
                  processedOptions[j] = True;
                  goto optionLoop;
                }
              } else
                goto failOptions;
            }
          }
        }
      }
      outMsg(stdErr, "unknown option: %s\n", opt);
      goto failOptions;
    } else
      break;
    optionLoop:;
  }

  // Process environment variable alternatives
  for (int jx = 0; jx < optionCount; jx++) {
    if (!processedOptions[jx] && options[jx].envVar != Null) {
      char *var = getenv(options[jx].envVar);
      if (var != Null) {
        switch (options[jx].hasArg) {
          case noArgument: {
            logical
              setOpt = (uniCmp(var, "true") == same || uniCmp(var, "TRUE") == same || uniCmp(var, "yes") == same ||
                        uniCmp(var, "YES") == same);
            if (options[jx].setter(var, setOpt, options[jx].cl) == Ok)
              processedOptions[jx] = True;
            else {
              outMsg(Stderr(), "bad environment variable: %s=%s\n", options[jx].envVar, var);
              goto failOptions;
            }
          }
          case hasArgument: {
            if (options[jx].setter(var, True, options[jx].cl) == Ok)
              processedOptions[jx] = True;
            else {
              outMsg(Stderr(), "bad environment variable: %s=%s\n", options[jx].envVar, var);
              goto failOptions;
            }
          }
        }

      }
    }
  }
  return ix;

  failOptions:

  showUsage(argv[0], copyRight, options, optionCount);
  return -1;
}

void showUsage(char *name, char *copyRight, Option options[], int optionCount) {
  ioPo stdErr = Stderr();

  if (copyRight != Null)
    outMsg(stdErr, "%s\n", copyRight);

  outMsg(stdErr, "Usage: %s\n", name);

  for (int ix = 0; ix < optionCount; ix++) {
    Option *opt = &options[ix];
    if (opt->helper != Null)
      opt->helper(stdErr, opt->shortName, opt->usage, opt->cl);
    else
      outMsg(stdErr, "    %s\n", options[ix].usage);
  }
}
