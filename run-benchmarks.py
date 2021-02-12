from numpy import arange
import subprocess

epi = 2
for p in range(3,8):
    n = 2**p
    for dum in arange(1-1.0/n,0.8,-1.0/n):
        for neu in range(0,int(n/5)+1):
            print('p {} dum {} epi {} neu {}'.format(n,dum,epi,neu))
            #subprocess.run(['srun','--cpus-per-task=16', '--mem=32G', '--time=200:00:00', '/nfs/home/mironovich/sbt/bin/sbt "run runtime bits:wm:constants --from {} --to {} --runs 20 --threads 0 --out triple/p-{}-d-{}-e-{}-n-{}.json --dummy {} --epi {} --neu {} --rug 0"'.format(p,p,p,dum,epi,neu,dum,epi,neu)])
            subprocess.run(['/nfs/home/mironovich/sbt/bin/sbt',"run runtime bits:wm:constants --from {} --to {} --runs 10 --threads 0 --out epi-two/p-{}-d-{}-e-{}-n-{}.json --dummy {} --epi {} --neu {} --rug 0".format(p,p,p,dum,epi,neu,dum,epi,neu)])
