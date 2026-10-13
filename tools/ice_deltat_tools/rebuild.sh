#!/bin/sh
# Rebuild the patched ICE from the original ICE.EXE (Beta 0.51) and fresh IERS data.
# needs: python3, nasm, pip packages capstone + astropy-iers-data (pip install -U astropy-iers-data
#        to pick up the latest Bulletin A before rebuilding)
set -e
cd "$(dirname "$0")"
python3 unexepack.py "${1:-ICE.EXE}" ICE_U.EXE      # -> ICE_U.EXE.img, ICE_U.EXE.rel.json
python3 make_table.py > /dev/null                    # -> data/dt_grid.txt
python3 gen_asm.py                                   # -> deltat_new.asm
nasm -f bin -o deltat_new.bin deltat_new.asm
python3 gen_fortran.py > /dev/null                   # -> DELTAT.FOR (matching Fortran)
python3 build_patch.py ICE_DT26.EXE
