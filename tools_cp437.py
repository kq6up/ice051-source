"""Copy the recovered sources (kept in UTF-8) for compilation.

  python3 tools_cp437.py SRC DST          gfortran build copy:
      code page 437, PAUSE 'msg' -> CALL MSPAUS('msg') (the MS FORTRAN
      console PAUSE, see scrio.c), FORM='BINARY' -> FORM='UNFORMATTED'
      (an MS FORTRAN extension; gfortran direct access is the same)
  python3 tools_cp437.py --dos SRC DST    MS FORTRAN 4.x copy:
      code page 437, CR LF line ends, INCLUDE 'x' -> $INCLUDE:'x'
"""
import sys, os, re, glob

args = sys.argv[1:]
dos = args[0] == '--dos'
if dos:
    args = args[1:]
src, dst = args
os.makedirs(dst, exist_ok=True)
for f in sorted(glob.glob(os.path.join(src, '*.for')) + glob.glob(os.path.join(src, '*.inc'))):
    t = open(f, encoding='utf-8').read()
    if dos:
        t = re.sub(r"^      INCLUDE '([^']*)'\s*$", r"$INCLUDE:'\1'", t, flags=re.M)
        t = t.replace('\n', '\r\n')
        name = os.path.basename(f).upper()
    else:
        t = t.replace("FORM='BINARY'", "FORM='UNFORMATTED'")
        t = re.sub(r"^(\s+)PAUSE\s+('.*')\s*$", r"\1CALL MSPAUS(\2)", t, flags=re.M)
        name = os.path.basename(f)
        if f.endswith('.inc'):
            continue                      # gfortran reads them from src/
    open(os.path.join(dst, name), 'wb').write(t.encode('cp437'))
