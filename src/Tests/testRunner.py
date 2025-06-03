import json
import re
from pprint import pprint
import subprocess
import sys, getopt

# Run a script for every member of the catalog

testDir = "/Users/fgm/Sources/star/src/Tests/"
tester = f"{testDir}/sbc-test"
tracing = False
ignore_failures = False

def main(argv):
    global tracing,tester
    
    pkgs = []

    try:
        opts,args = getopt.getopt(argv,"",["test=",
                                           "help",
                                           "tester=",
                                           "all",
                                           "ignore-failures",
                                           "tracing"])
    except getopt.GetoptError as e:
        print (e.msg)
        sys.exit(2)
    for opt, arg in opts:
        if opt == '--help':
            print (usage)
            sys.exit()
        elif opt in ["--tester"]:
            tester = arg
        elif opt in ["--test"]:
            pkgs.append(arg)
        elif opt == '--all':
            print ("Open ",testDir+'catalog')
            with open(testDir+'catalog') as cat:
                catalog = json.load(cat)
                
                for pk in catalog["content"]:
                    pkgs.append(pk)
        elif opt == "--ignore-failures":
            ignore_failures = True
        elif opt in ['-d','--tracing']:
            tracing = True
    
    print ("Run tests on ",pkgs)
    failures = []
    for pkg in pkgs:
        print ("Testing:",pkg)

        return_code = runTest(pkg)

        if return_code!=0:
            print ("test on ",pkg," failed")
            if not ignore_failures:
                sys.exit(1)
            else:
                failures.append(pkg)
    if failures != []:
        print (failures, "tests failed to complete")


def runTest(Pkg):
    "Run a test on a package from using a script"
    global tester
    
    if tracing:
        print ("perform ",tester, Pkg)
    try:
        out = subprocess.check_output([tester,Pkg],
                                      stderr=subprocess.STDOUT)
        if tracing:
            print(out.decode('utf-8'))
        return 0
    except subprocess.CalledProcessError as err:
        print('\033[31m'+err.output.decode('utf-8')+'\033[0m')
        return err.returncode

if __name__ == "__main__":
    main(sys.argv[1:])
