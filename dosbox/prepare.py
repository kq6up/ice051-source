#!/usr/bin/env python3
"""Prepare an isolated DOS drive; native Linux/macOS builds are untouched."""
import argparse
import shutil
import zipfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--source', type=Path, default=ROOT.parent,
                    help='ICE repository root')
parser.add_argument('--drive', type=Path, default=ROOT / 'drive_c',
                    help='new DOS drive folder (default: dosbox/drive_c)')
parser.add_argument('--fortran', type=Path,
                    help='installed FORTRAN folder containing BIN and LIB; includes MASM')
args = parser.parse_args()
source = args.source.resolve()
drive = args.drive.resolve()
if drive.exists():
    raise SystemExit('Drive already exists; choose a new --drive directory to preserve its files.')
if not (source / 'dos/NAVIG.FOR').is_file():
    raise SystemExit('Cannot find preserved DOS sources; set --source to the ICE repository.')
if args.fortran:
    compiler = args.fortran.resolve()
    for required in ['BIN/FL.EXE', 'BIN/MASM.EXE', 'BIN/LINK.EXE', 'LIB/LLIBFORE.LIB']:
        if not (compiler / required).is_file():
            raise SystemExit('Missing compiler component: ' + str(compiler / required))

with zipfile.ZipFile(source / 'reference/ice.zip') as archive:
    # Read the DOS data directly; GNU Fortran's reframed data is incompatible.
    records = {Path(item.filename).name.upper(): archive.read(item)
               for item in archive.infolist() if not item.is_dir()}
    for folder in ['ICE', 'ICE26', 'ORIGINAL']:
        target = drive / folder
        target.mkdir(parents=True)
        for name, data in records.items():
            (target / name).write_bytes(data)
        # Fixed-width defaults for the confirmed October 13, 2026 Sun test.
        defaults = ''.join(' ' * 17 + f'{v:15.6f}\r\n'
                           for v in [20261013.23, 34, -117, 1])
        defaults += ''.join(' ' * 23 + f'{v:3d}\r\n' for v in [1, 0])
        (target / 'ICE.DFT').write_bytes(defaults.encode('ascii'))

for folder, binary, link in [('ICE', 'ice-vintage.exe', 'ICE.LNK'),
                             ('ICE26', 'ice-dt2026.exe', 'ICE26.LNK')]:
    target = drive / folder
    for item in (source / 'dos').iterdir():
        if item.is_file():
            shutil.copy2(item, target / item.name)
    shutil.copy2(ROOT / 'bin' / binary, target / 'ICE.EXE')
    shutil.copy2(ROOT / 'batch' / link, target / 'ICE.LNK')
    if folder == 'ICE26':
        path = target / 'NAVIG.FOR'
        data = path.read_bytes()
        old = b'DBLE (ID) / 30.D0'
        if data.count(old) != 1:
            raise SystemExit('Expected exactly one historical NAVIG decimal-year expression.')
        path.write_bytes(data.replace(old, b'DBLE (ID) / 365.25D0'))

for item in (ROOT / 'batch').iterdir():
    if item.suffix.upper() != '.LNK':
        shutil.copy2(item, drive / item.name)
(drive / 'LOGS').mkdir()
if args.fortran:
    shutil.copytree(compiler, drive / 'FORTRAN')
print('Prepared ' + str(drive))
print('ICE: rebuilt vintage model; ICE26: updated 2026 model; ORIGINAL: historical reference.')
