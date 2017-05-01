#!/usr/bin/python

import fcntl, json, os, requests, select, shutil, socket, struct, subprocess, sys, time
from circleclient import circleclient

username = 'markfirmware'
project = 'ultibo-webstatus'
branch = 'test-20170425'
ports = ['5080:80']

def getbuild (circle, username, project, branch):
    global artifacts, kernelpath, buildnumber
    builds = circle.build.recent (username, project, branch=branch)
    build = builds [0]
    buildnumber = build ['build_num']
    print username, project, 'build', buildnumber, build ['status']
    if not (build ['status'] in ['fixed', 'success']):
        return False
    artifacts = circle.build.artifacts (username, project, buildnumber)
    artifacts = sorted (artifacts, key = lambda a: a ['pretty_path'])
    e = os.path.join ('artifacts', 'build-' + str (buildnumber))
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
    return True

def get_ip_address(ifname):
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    return socket.inet_ntoa(fcntl.ioctl(
        s.fileno(),
        0x8915,  # SIOCGIFADDR
        struct.pack('256s', ifname[:15])
    )[20:24])

def runqemu (kernelpath):
    global buildnumber
    cmdline = 'qemuhostip={} username={} project={} branch={} buildnumber={}'.format (get_ip_address ('eth0'), username, project, branch, buildnumber)
    qemu = subprocess.Popen (["qemu-system-arm",
                              "-M", "versatilepb",
                              "-cpu", "cortex-a8",
                              "-kernel", kernelpath,
                              "-append", cmdline,
                              "-m", "256M",
                              "-serial", "stdio",
                              "-usb",
                              "-net", "nic",
                              "-net", "user,hostfwd=tcp::5080-:80",
                              "-vnc", ":70,websocket"],
                             stdin=subprocess.PIPE,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
    while qemu.poll () == None:
        line = qemu.stdout.readline ()
        print line,
        if 'program stop' in line:
            break
        if 'system reset requested' in line:
            print 'detected system reset request'
            (a, b, c) = select.select ([qemu.stdout], [], [], 3.0)
            if a == []:
                print 'system reset seems to have failed - restarting qemu'
                while True:
                    (a, b, c) = select.select ([qemu.stderr], [], [], 3.0)
                    if a == []:
                        break
                    print qemu.stderr.read (1),
                print
                print
                qemu.terminate ()
        time.sleep (0.01)

def main ():
    global kernelpath
    circle = circleclient.CircleClient ('')
    while True:
        while not getbuild (circle, username, project, branch):
            time.sleep (30)
        runqemu (kernelpath)

if __name__ == "__main__":
    main ()
