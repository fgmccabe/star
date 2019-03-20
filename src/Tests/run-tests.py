import json
from pprint import pprint
import subprocess
import sys, getopt


root = "../../"
execRoot = root+"cmake-build-debug/"
usage = "runtests.py -t <testpkg>"

tracing = False


def main(argv):
    global tracing

    pkgs = []
    compile_only = False
    try:
        opts,args = getopt.getopt(argv,"dhct:",["test=","help","compile_only","tracing","heap","all"])
    except getopt.GetopError:
        print usage
        sys.exit(2)
    for opt, arg in opts:
        if opt== '--help':
            print usage
            sys.exit()
        elif opt in ["-t","--test"]:
            pkgs.append(arg)
        elif opt in ['-c','--compile_only']:
            compile_only = True
        elif opt in ['-d','--tracing']:
            tracing = True
        elif opt == '--all':
            with open('catalog') as cat:
                catalog = json.load(cat)

                for pk in catalog["content"]:
                    pkgs.append(pk)
        
    print "Run tests on ",pkgs
    for pkg in pkgs:
        print "Testing:",pkg

        return_code = bootCompile(pkg)
        if return_code!=0:
            print "compiling ",pkg," failed"
        elif not compile_only:
            return_code = runPkg(pkg)
            print "return code from running ",pkg," is ",return_code

def bootCompile(Pkg):
    "Compile a package using the bootstrap compiler"
    if tracing:
        print "compile:",root+"src/BootCompiler/sbc","-r",root+"src/Star/Build/", "-w",root+"src/Tests/", "-g", Pkg
    return subprocess.call([root+"src/BootCompiler/sbc",
                            "-r",root+"src/Star/Build/",
                            "-w",root+"src/Tests/",
                            "-g",
                            Pkg])

def runPkg(Pkg):
    "Run a previously compiled package"
    if tracing:
        print "run:",execRoot+"src/Engine/star","-r",root+"src/Star/Build/", "-h","4m", Pkg
    return subprocess.call([execRoot+"src/Engine/star",
                            "-r",root+"src/Star/Build/",
                            "-h","4m",
                            Pkg])

if __name__ == "__main__":
    main(sys.argv[1:])
            
            

