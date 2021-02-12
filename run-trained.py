import json
import subprocess

# from and to are IDs of lines in model_results.json, if to == 0, it uses len(model_results) as to parameter
command = "run runtime bits:wm:tuned:json --runs 20 --threads 0 --from 0 --to 0"
# note the sbt executable
subprocess.run(['/nfs/home/mironovich/sbt/bin/sbt',command])
