#!/bin/sh
# Build the recovered ICE 0.51 with gfortran (Linux / macOS); result: build/ice
#   ./build.sh               original program (1988 Delta T polynomials)
#   ./build.sh --deltat2026  with the 2026 Delta T table and the corrected
#                            decimal year in F7 Navigation (= ICE_DT26.EXE)
set -e
cd "$(dirname "$0")"
mkdir -p build
rm -f build/*.for
python3 tools_cp437.py src build
if [ "$1" = "--deltat2026" ]; then
    rm build/deltat.for
    python3 tools_cp437.py src/alt build
    sed -i.bak 's|DBLE (ID) / 30.D0|DBLE (ID) / 365.25D0|' build/navig.for
    rm -f build/navig.for.bak
fi
gcc -O1 -c -o build/scrio.o src/scrio.c
FFLAGS="-ffixed-line-length-none -I src -std=legacy -O0 -fno-automatic -w"
# (the sources keep to 72 columns; the gfortran adaptations made by
#  tools_cp437.py may lengthen a line, hence -ffixed-line-length-none)
# x87 arithmetic is closest to the original 8087 code
case "$(uname -m)" in i?86|x86_64|amd64) FFLAGS="$FFLAGS -mfpmath=387" ;; esac
gfortran $FFLAGS -o build/ice build/*.for build/scrio.o
