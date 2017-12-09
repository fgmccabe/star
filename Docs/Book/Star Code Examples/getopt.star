getopt is package {


 implementation coercion over (string, list of string) is {
   coerce(s) is list of {s};
 }

 main has type action (list of string);
 main(args) {
   logMsg(info, "args: $(args)");
 }
}