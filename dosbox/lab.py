#!/usr/bin/env python3
"""Build, test, or open the isolated ICE DOS experiment."""
import argparse
import shutil
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parent
DRIVE = ROOT / 'drive_c'


def dos(commands, headless=True, log='HOST.LOG'):
    executable = shutil.which('dosbox-x')
    if not executable:
        raise SystemExit('DOSBox-X is required. This lab uses the installed native emulator.')
    args = [executable, '-conf', str(ROOT / 'dosbox-x.conf'),
            '-nopromptfolder', '-fastlaunch']
    if headless:
        args += ['-silent']
    args += ['-c', 'mount c "' + str(DRIVE) + '"', '-c', 'c:']
    for command in commands:
        args += ['-c', command]
    if not headless:
        return subprocess.call(args)

    try:
        run = subprocess.run(args, capture_output=True, timeout=55)
    except subprocess.TimeoutExpired as error:
        (DRIVE / 'LOGS' / log).write_bytes((error.stdout or b'') + (error.stderr or b''))
        return 124
    (DRIVE / 'LOGS' / log).write_bytes(run.stdout + run.stderr)
    return run.returncode


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('action', choices=['build', 'test', 'launch'])
models = parser.add_mutually_exclusive_group()
models.add_argument('--original', action='store_true', help='supplied historical ICE.EXE')
models.add_argument('--deltat2026', action='store_true', help='rebuilt updated Delta T model')
args = parser.parse_args()
folder = 'ORIGINAL' if args.original else 'ICE26' if args.deltat2026 else 'ICE'

if args.action == 'build':
    if args.original:
        raise SystemExit('The historical reference is preserved; build ICE or ICE26 instead.')
    batch = 'BUILD26.BAT' if args.deltat2026 else 'BUILD.BAT'
    (DRIVE / folder / 'ICE.EXE').unlink(missing_ok=True)
    code = dos(['call ' + batch], log=folder + '-BUILD-HOST.LOG')
    if code != 0 or not (DRIVE / folder / 'ICE.EXE').exists():
        raise SystemExit('Build failed; inspect drive_c/LOGS. Emulator status: ' + str(code))
    print('Built ' + folder + '/ICE.EXE', flush=True)
elif args.action == 'launch':
    raise SystemExit(dos(['set PATH=C:\\FORTRAN\\BIN;Z:\\',
                         'set LIB=C:\\FORTRAN\\LIB', 'cd ' + folder, 'ICE.EXE'],
                        headless=False))
else:
    for selected, batch in [('ICE', 'RUNNEW'), ('ICE26', 'RUN26'), ('ORIGINAL', 'RUNOLD')]:
        (DRIVE / selected / 'SUN.OUT').unlink(missing_ok=True)
        code = dos(['call ' + batch + '.BAT'], log=batch + '-HOST.LOG')
        result = DRIVE / selected / 'SUN.OUT'
        if code != 0 or not result.exists() or not result.stat().st_size:
            raise SystemExit(selected + ' failed; inspect drive_c/LOGS. Emulator status: ' + str(code))
        data = result.read_bytes()
        if b'2026 Oct 13 at 23 00 00' not in data or b'239.2' not in data:
            raise SystemExit(selected + ' did not complete the expected Sun calculation.')
        print(selected + ': Sun test completed; emulator exited normally.', flush=True)
    rebuilt = (DRIVE / 'ICE/SUN.OUT').read_bytes()
    original = (DRIVE / 'ORIGINAL/SUN.OUT').read_bytes()
    if rebuilt != original:
        raise SystemExit('Rebuilt original-model output differs from the historical reference.')
    updated = (DRIVE / 'ICE26/SUN.OUT').read_bytes()
    if b'69.2 seconds' not in updated:
        raise SystemExit('Updated model did not report the expected Delta T.')
    print('Original-model output matches the historical executable byte for byte.')
    print('Updated model reports Delta T = 69.2 seconds.')
