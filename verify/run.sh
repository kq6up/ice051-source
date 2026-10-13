#!/bin/sh
# Compare the recovered program with the original, screen by screen.
#   verify/run.sh /path/to/ice_directory [name-filter]
# Needs Python 3 with unicorn, capstone and numpy (pip install unicorn
# capstone numpy), gfortran and gcc.
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
ICE=$1; shift
python3 "$HERE/prepare.py" "$ICE/ICE.EXE"
rm -rf "$HERE/orig" "$HERE/port"
mkdir "$HERE/orig"
cp "$ICE"/* "$HERE/orig/"
python3 "$HERE/../setup_data.py" "$ICE" "$HERE/port" > /dev/null
(cd "$HERE/.." && ./build.sh)
python3 "$HERE/e2e_suite.py" "$@"
