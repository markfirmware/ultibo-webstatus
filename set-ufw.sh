#/bin/bash
set -x

ufw allow 8$1
ufw allow 577$1
ufw allow 597$1
