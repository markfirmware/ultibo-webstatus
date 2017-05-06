#!/usr/bin/python

import fcntl, json, os, requests, select, shutil, socket, struct, subprocess, sys, time
from circleclient import circleclient

portdigit = sys.argv [1]

username = 'markfirmware'
project = 'ultibo-webstatus'
branch = 'test-20170425'
ports = 'hostfwd=tcp::8' + portdigit + '-:80'

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
    e = os.path.join ('instance-' + portdigit, 'artifacts', 'build-' + str (buildnumber))
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
    global buildnumber, qemu, qemuhostlocation
    cmdline = 'NETWORK0_IP_CONFIG=STATIC NETWORK0_IP_ADDRESS=10.0.2.10{} NETWORK0_IP_NETMASK=255.255.255.0 NETWORK0_IP_GATEWAY=10.0.2.1 qemuhostlocation={} qemuhostip={} qemuhostportdigit={} username={} project={} branch={} buildnumber={}'.format (portdigit, qemuhostlocation, get_ip_address ('eth0'), portdigit, username, project, branch, buildnumber)
    qemu = subprocess.Popen (["qemu-system-arm",
                              "-M", "versatilepb",
                              "-cpu", "cortex-a8",
                              "-kernel", kernelpath,
                              "-append", cmdline,
                              "-m", "96M",
                              "-serial", "stdio",
                              "-usb",
                              "-net", "nic,macaddr=52:54:00:12:34:5" + portdigit,
                              "-net", "socket,mcast=230.0.0.1:1234",
                              "-net", "user," + ports,
                              "-vnc", ":7" + portdigit + ",websocket"],
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
            time.sleep (1)
            if not waitforstart ():
                print 'system reset seems to have failed - restarting qemu'
                break
        time.sleep (0.01)

def waitforstart ():
    global qemu
    linebuffer = ''
    started = False
    while not started and ([], [], []) != select.select ([qemu.stdout], [], [], 3.0):
        try:
            data = qemu.stdout.read (1)
        except:
            break
        if data == "":
            break
        for c in data:
            if c == '\r':
                pass
            elif c == '\n':
                print linebuffer
                if 'program start' in linebuffer:
                    started = True
                linebuffer = ''
            else:
                linebuffer += c
    return started

def main ():
    global kernelpath, qemu, qemuhostlocation
    qemuhostlocation=''
    try:
        qemuhostlocation = os.environ ['HOSTLOCATION']
    except:
        pass
    circle = circleclient.CircleClient ('')
    while True:
        while not getbuild (circle, username, project, branch):
            time.sleep (30)
        try:
            runqemu (kernelpath)
        except:
             pass
        try:
            qemu.terminate ()
        except:
            print 'exception terminating qemu'
        try:
            qemu.wait ()
        except:
            print 'exception waiting for qemu'
        print 'qemu done'

if __name__ == "__main__":
    main ()
