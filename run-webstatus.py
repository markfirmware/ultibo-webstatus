#!/usr/bin/python

import git, json, os, parse, requests, select, shutil, socket, subprocess, sys, time
from circleclient import circleclient

username = 'markfirmware'
project = 'ultibo-webstatus'
branch='test-20170425'

def getbuild (username, project, branch):
    global artifacts, circle, kernelpath, lastbuildnum
    builds = circle.build.recent (username, project, branch=branch)
    build = builds [0]
    build_num = build ['build_num']
    if build_num == lastbuildnum:
        return False
    print username, project, 'build', build_num, build ['status']
    if not (build ['status'] in ['fixed', 'success']):
        return False
    artifacts = circle.build.artifacts (username, project, build_num)
    artifacts = sorted (artifacts, key = lambda a: a ['pretty_path'])
    e = os.path.join ('artifacts', 'build-' + str (build_num))
    if os.path.exists (e):
        shutil.rmtree (e)
    for a in artifacts:
        r = requests.get (a ['url'])
        parts = a ['pretty_path'].split (os.sep, 1)
        a ['short_path'] = parts [1]
        p = os.path.join (e, a ['short_path'])
        if os.path.basename (p) == 'kernel.bin':
            kernelpath = p
            print 'kernel', kernelpath
        print a ['short_path']
        d = os.path.dirname (p)
        if not os.path.exists (d):
            os.makedirs (d)
        with open (p, 'wb') as fd:
            for chunk in r.iter_content (chunk_size=4096):
                fd.write (chunk)
    lastbuildnum = build_num
    return True

def main ():
    global circle, kernelpath, lastbuildnum
    circle = circleclient.CircleClient ('')
    lastbuildnum = 0
    while not getbuild (username, project, branch):
        time.sleep (30)
    qemuprocess = subprocess.call (["qemu-system-arm",
                                    "-M", "versatilepb",
                                    "-cpu", "cortex-a8",
                                    "-kernel", kernelpath,
                                    "-m", "256M",
                                    "-usb",
                                    "-net", "nic",
                                    "-net", "user,hostfwd=tcp::5080-:80",
                                    "-display", "none"])
    print "qemu started"

if __name__ == "__main__":
    main ()
