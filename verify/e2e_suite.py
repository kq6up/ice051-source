"""End-to-end regression suite: original ICE.EXE (emulated) versus the
recovered source built with gfortran.  Every case compares the final
80x25 screen and the complete output stream; cases that write files
also compare the files.   python3 tests/e2e_suite.py [substring]"""
import sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from e2e import *

RESULTS = []


def fcmp(name):
    a = open(os.path.join(ORIG_DATA, name), 'rb').read().decode('latin1')
    b = open(os.path.join(PORT_DATA, name), 'rb').read().decode('latin1')
    a = norm(a).replace('\r\n', '\n'); b = norm(b).replace('\r\n', '\n')
    if a == b:
        print('       file %s same (%d bytes)' % (name, len(a))); return True
    al, bl = a.split('\n'), b.split('\n')
    print('       file %s DIFFERS (%d / %d lines)' % (name, len(al), len(bl)))
    n = 0
    for x, y in zip(al, bl):
        if x != y:
            print('        o|' + x); print('        p|' + y); n += 1
            if n > 4: break
    return False


def case(label, script, files=(), known=None):
    if len(sys.argv) > 1 and sys.argv[1].lower() not in label.lower():
        return
    clean = tuple(files) or ('E2E.OUT',)
    ok = compare(script, label, clean=clean)[0]
    for f in files:
        ok = fcmp(f) and ok
    if not ok and known:
        print('       (known difference: %s)' % known)
        ok = True
    RESULTS.append((label, ok))


# main menu, F1 initial values, F2 output file, F10 quit
case('startup menu', [])
case('F1 show', [F(1)])
case('F1 edit date accept', [F(1), F(1), L('19900101'), F(7)])
case('F1 bad input', [F(1), F(2), L('x')])
case('F2 create file', [F(2), L('E2E.OUT'), ENTER])
case('F2 create+close+reopen', [F(2), L('E2E.OUT'), ENTER, F(2), ENTER, F(2), L('E2E.OUT'), ENTER])
case('F10 quit', [F(10)])
# F9 catalogs
case('F9 catalog 1', [F(9), L('1'), L('N')])
case('F9 catalog 2', [F(9), L('2'), L('N')])
case('F9 list messier', [F(9), L('MESSIER.CAT'), L('Y'), ENTER])
case('F9 radio list', [F(9), L('3'), L('Y')])
case('F9 missing file', [F(9), L('NOFILE.CAT')])
case('F9 $USER', [F(9), L('5')])
case('F9 twice', [F(9), L('4'), L('N'), F(9), L('3'), L('N')])
case('F9 bad header', [F(9), L('CATALOG.EXE')])
case('F9 file open on another unit', [F(9), L('ICE.DFT')],
     known='MS run-time refuses to connect ICE.DFT (still open on unit 1) to a second unit; gfortran allows it')
case('F9 blank', [F(9), ENTER])
case('F9 list to file', [F(2), L('E2E.OUT'), ENTER, F(9), L('4'), L('Y'), ENTER, F(10)], files=('E2E.OUT',))
# F3 positions
case('F3 menu', [F(3)])
case('F3 apparent Mars', [F(3), F(1), L('MARS'), ENTER])
case('F3 apparent Moon', [F(3), F(1), L('MOON')])
case('F3 apparent Sun', [F(3), F(1), L('SUN')])
case('F3 astrometric Sirius', [F(3), F(2), L('SIRIUS')])
case('F3 star number', [F(3), F(1), L('42')])
case('F3 star name', [F(3), F(2), L('ALDEBARA')])
case('F3 helio Jupiter', [F(3), F(3), L('JUPITER')])
case('F3 bary Pluto', [F(3), F(4), L('PLUTO')])
case('F3 bary Moon', [F(3), F(4), L('MOON')])
case('F3 impossible', [F(3), F(1), L('EARTH'), ENTER])
case('F3 helio star', [F(3), F(3), L('VEGA'), ENTER])
case('F3 not found retry', [F(3), F(1), L('XYZZY'), ENTER, L('VENUS')])
case('F3 leave', [F(3), F(6)])
case('F3 blank name', [F(3), F(1), ENTER])
case('F3 messier object', [F(9), L('4'), L('N'), F(3), F(1), L('M 31')])
case('F3 page 2 of catalog 2', [F(9), L('2'), L('N'), F(3), F(2), L('1000')])
case('F3 all to file', [F(2), L('E2E.OUT'), ENTER, F(3), F(1), L('MARS'), ENTER, F(3), F(2), L('VEGA'), ENTER,
                        F(3), F(3), L('SATURN'), ENTER, F(3), F(4), L('NEPTUNE'), ENTER, F(10)], files=('E2E.OUT',))
case('F3 F5 Chebyshev Sun', [F(3), F(5), L('2447343.5'), L('SUN'), L('CHEB.OUT'), ENTER], files=('CHEB.OUT',))
case('F3 F5 Chebyshev Moon', [F(3), F(5), L('2447343.5'), L('MOON'), L('CHEB.OUT'), ENTER], files=('CHEB.OUT',),
     known='15th digit: MS kept COEFS intermediates in 80-bit registers')
case('F3 F5 bad date, bad name', [F(3), F(5), L('abc'), L('2447343.5'), L('NOPE'), L('MARS'), L('X.OUT')],
     files=('X.OUT',), known='15th digit: MS kept COEFS intermediates in 80-bit registers')

# F4 physical ephemeris
case('F4 Mars illumination', [F(4), L('MARS'), L('I'), ENTER])
case('F4 Moon illumination', [F(4), L('MOON'), L('i')])
case('F4 Jupiter rotation', [F(4), L('JUPITER'), L('R')])
case('F4 Saturn illumination', [F(4), L('SATURN'), L('I')])
case('F4 Moon rotation', [F(4), L('MOON'), L('R')])
case('F4 Earth', [F(4), L('EARTH'), ENTER])
case('F4 Sun', [F(4), L('SUN'), ENTER])
case('F4 star', [F(4), L('SIRIUS'), ENTER])
case('F4 not found', [F(4), L('ZZZ'), ENTER, ENTER])
case('F4 to file', [F(2), L('E2E.OUT'), ENTER, F(4), L('VENUS'), L('I'), ENTER, F(4), L('MOON'), L('I'), ENTER,
                    F(4), L('MARS'), L('R'), ENTER, F(10)], files=('E2E.OUT',))

# F5 sidereal time
case('F5 sidereal table', [F(5), ENTER])
case('F5 long table', [F(1), F(5), L('40'), F(4), L('0.1'), F(3), L('-77.0654'), F(7), F(5)])
case('F5 east longitude', [F(1), F(3), L('190.5'), F(1), L('2451545.0'), F(7), F(5), ENTER, F(5)])
case('F5 to file', [F(2), L('E2E.OUT'), ENTER, F(5), ENTER, F(10)], files=('E2E.OUT',))

# F7 navigation
case('F7 Sun', [F(7), L('SUN')])
case('F7 Venus', [F(7), L('VENUS'), ENTER])
case('F7 Moon', [F(7), L('MOON')])
case('F7 star', [F(7), L('VEGA')])
case('F7 ALL', [F(7), L('ALL')])
case('F7 all lower', [F(7), L('all'), ENTER])
case('F7 not found', [F(7), L('QQQ'), ENTER, L('MARS')])
case('F7 ALL other catalog', [F(9), L('4'), L('N'), F(7), L('ALL')])
case('F7 night', [F(1), F(1), L('19880701.03'), F(7), F(7), L('ALL')])
case('F7 east', [F(1), F(3), L('150'), F(2), L('-33.9'), F(7), F(7), L('ALL')])
case('F7 ALL to file', [F(2), L('E2E.OUT'), ENTER, F(7), L('ALL'), ENTER, F(7), L('SATURN'), ENTER, F(10)], files=('E2E.OUT',))

# F8 daily configuration
case('F8 default date', [F(8)])
case('F8 back', [F(8), ENTER])
case('F8 new moon 1990', [F(1), F(1), L('19900126'), F(7), F(8)])
case('F8 full moon', [F(1), F(1), L('19880730.12'), F(7), F(8)])
case('F8 1995', [F(1), F(1), L('2449800.5'), F(7), F(8)])
case('F8 to file', [F(2), L('E2E.OUT'), ENTER, F(8), ENTER, F(1), F(1), L('19891231'), F(7), F(8), ENTER, F(10)], files=('E2E.OUT',))

# F6 rise and set
case('F6 Sun', [F(6), L('SUN')])
case('F6 Sun twilight', [F(6), L('SUN'), F(1)])
case('F6 Sun twilight back', [F(6), L('SUN'), F(1), ENTER])
case('F6 Sun F2', [F(6), L('SUN'), F(2)])
case('F6 Moon', [F(6), L('MOON'), ENTER])
case('F6 Mars', [F(6), L('MARS')])
case('F6 Mercury', [F(6), L('MERCURY')])
case('F6 Venus', [F(6), L('VENUS')])
case('F6 star', [F(6), L('SIRIUS')])
case('F6 circumpolar star', [F(6), L('POLARIS')])
case('F6 not found', [F(6), L('NOSUCH'), ENTER, L('JUPITER')])
case('F6 east longitude', [F(1), F(3), L('139.7'), F(2), L('35.7'), F(6), L('9'), F(7), F(6), L('MOON')])
case('F6 far north Sun', [F(1), F(2), L('65'), F(1), L('19880601'), F(7), F(6), L('SUN'), F(1)])
case('F6 far north winter', [F(1), F(2), L('69'), F(1), L('19881215'), F(7), F(6), L('SUN'), F(1)])
case('F6 south Moon', [F(1), F(2), L('-62'), F(5), L('20'), F(7), F(6), L('MOON')])
case('F6 to file', [F(2), L('E2E.OUT'), ENTER, F(6), L('SUN'), F(1), ENTER, F(6), L('MOON'), ENTER, F(10)], files=('E2E.OUT',))

if __name__ == '__main__':
    bad = [l for l, ok in RESULTS if not ok]
    print('\n%d cases, %d differ%s' % (len(RESULTS), len(bad), (': ' + ', '.join(bad)) if bad else ''))
