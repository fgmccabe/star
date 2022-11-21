import json
from pprint import pprint
import subprocess
import sys, getopt

starExec = "/Users/fgm/Projects/star/Build/src/Engine/star"
testDir = "/Users/fgm/Projects/star/src/Tests/"
sbc = "/Users/fgm/Projects/star/src/BootCompiler/sbc"
sc = "/Users/fgm/bin/sc"
repoDir = "/Users/fgm/Projects/star/Build/.star-repo/"
usage = "runtests.py [-t <testpkg>|--all] --compile_only --tracing --repo <dir>"

tracing = False

def main(argv):
    global tracing,starExec,sbc,repoDir,testDir

    pkgs = []
    compile_only = False
    ignore_failures = False
    compiler=bootCompile
    compile_failures = []
    run_failures = []
    try:
        opts,args = getopt.getopt(argv,"dhct:",["sc","test=","help","compile_only","tracing","all","ignore_failures","starexec=","sbc","testdir=","repo="])
    except getopt.GetoptError as e:
        print (e.msg)
        sys.exit(2)
    for opt, arg in opts:
        if opt== '--help':
            print (usage)
            sys.exit()
        elif opt in ["-t","--test"]:
            pkgs.append(arg)
        elif opt in ['-c','--compile_only','--compile-only']:
            compile_only = True
        elif opt in ['-d','--tracing']:
            tracing = True
        elif opt == "--ignore_failures":
            ignore_failures = True
        elif opt == '--all':
            with open(testDir+'catalog') as cat:
                catalog = json.load(cat)

                for pk in catalog["content"]:
                    pkgs.append(pk)
        elif opt == "--starexec":
            starExec = arg
        elif opt=="--sbc":
            compiler=bootCompile
        elif opt=="--repo":
            repoDir = arg
        elif opt=="--testdir":
            testDir = arg
        elif opt=="--sc":
            compiler=mainCompile

    print ("Run tests on ",pkgs)
    for pkg in pkgs:
        print ("Testing:",pkg)

        return_code = compiler(pkg)
        if return_code!=0:
            print ("compiling ",pkg," failed")
            if not ignore_failures:
                sys.exit(1)
            else:
                run_failures.append(pkg)
        elif not compile_only:
            return_code = runPkg(pkg)
            if return_code!=0:
                if not ignore_failures:
                    print ("return code from running ",pkg," is ",return_code)
                    sys.exit(return_code)
                else:
                    run_failures.append(pkg)
    if compile_failures != []:
        print (compile_failures, "tests failed to compile")
    if run_failures != []:
        print (run_failures, "tests failed to complete")

def bootCompile(Pkg):
    "Compile a package using the bootstrap compiler"
    if tracing:
        print ("compile:",sbc,"-r",repoDir, "-w", testDir, "-g", Pkg)
    return subprocess.call([sbc, "-r",repoDir, "-w",testDir, "-g", Pkg])

def mainCompile(Pkg):
    "Compile a package using the main compiler"
    if tracing:
        print ("compile:",sc,"-R",repoDir, "-W", testDir, Pkg)
    return subprocess.call([sc, "-R",repoDir, "-W",testDir, Pkg])

def runPkg(Pkg):
    "Run a previously compiled package"
    if tracing:
        print ("run:",starExec,"-r",repoDir, "-h","4m", Pkg)

    try:
        out = subprocess.check_output([starExec, "-r",repoDir,"-h","4m", Pkg],
                                      stderr=subprocess.STDOUT)
        if tracing:
            print(out)
        return 0
    except subprocess.CalledProcessError as err:
        print(err.output)
        return err.returncode

if __name__ == "__main__":
    main(sys.argv[1:])

