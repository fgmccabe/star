import json
from pprint import pprint
import subprocess

with open('catalog') as cat:
    catalog = json.load(cat)

pkgs = catalog["content"]

for pkg in pkgs:
    print("Test pkg {}".format(pkg))
    return_code = subprocess.call(["../BootCompiler/sbc","-r","../Star/Build/","-w","../Tests/","-g",pkg])
#    pprint(return_code)
    if return_code!=0:
        exit(return_code)
    
