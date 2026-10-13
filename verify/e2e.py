"""End-to-end comparison: original ICE.EXE (Unicorn DOS emulator) versus
the recovered source built with gfortran (build/ice), same key script.
Script tokens: ('key', asc, scan)  -> consumed by GETKEY
               ('line', text)      -> typed line (READ / PAUSE)
Screens are rendered with the same 80x25 teletype model."""
import os, sys, subprocess, shutil, tempfile
HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, HERE)
import dosemu
# original program files (MS FORTRAN data formats) and the converted copy
ORIG_DATA = os.environ.get('ICE_ORIG_DATA', os.path.join(HERE, 'orig'))
PORT_DATA = os.environ.get('ICE_PORT_DATA', os.path.join(HERE, 'port'))

def F(n): return ('key', 0, 0x3a + n)
def K(ch): return ('key', ord(ch), 0)
def L(text): return ('line', text)
ENTER = ('line', '')

class Screen:
    def __init__(self):
        self.rows = [[' '] * 80 for _ in range(25)]; self.r = 0; self.c = 0
    def put(self, ch):
        if ch == '\r': self.c = 0; return
        if ch == '\n': self.r += 1
        elif ch == '\b': self.c = max(0, self.c - 1); return
        elif ch == '\x07': return
        else:
            self.rows[self.r][self.c] = ch; self.c += 1
            if self.c >= 80: self.c = 0; self.r += 1
        if self.r >= 25:
            self.rows.pop(0); self.rows.append([' '] * 80); self.r = 24
    def feed(self, data):
        i = 0
        while i < len(data):
            ch = data[i]
            if ch == '\x1b' and i + 1 < len(data) and data[i + 1] == '[':
                j = i + 2
                while j < len(data) and not data[j].isalpha() and data[j] not in '}]': j += 1
                body, cmd = data[i + 2:j], data[j]
                if cmd == 'H':
                    a, b = body.split(';'); self.r, self.c = int(a) - 1, int(b) - 1
                elif cmd == '}':
                    t, l, b, r = (int(x) - 1 for x in body.split(';'))
                    for y in range(t, b + 1):
                        for x in range(l, r + 1): self.rows[y][x] = ' '
                    self.r, self.c = t, l
                elif cmd == ']' or (cmd == 'K' and body.startswith('KEYS')):
                    pass
                i = j + 1; continue
            self.put(ch); i += 1
    def text(self):
        return '\n'.join(''.join(r).rstrip() for r in self.rows)

def run_orig(script, datadir=None):
    keys = []
    for t in script:
        if t[0] == 'key': keys.append((t[1], t[2]))
        else: keys += [(ord(c), 0) for c in t[1]] + [(13, 0x1c)]
    img = os.environ.get('ICE_ORIG_IMAGE')
    d = dosemu.DOS(image=open(img, 'rb').read() if img else None, keys=keys, datadir=datadir or ORIG_DATA)
    try: msg = d.run()
    except Exception as e: msg = repr(e)
    s = Screen(); s.feed(''.join(d.transcript))
    return s, ''.join(d.transcript), msg

def run_port(script, datadir=None, exe=os.path.join(ROOT, 'build', 'ice')):
    tmp = tempfile.mkdtemp()
    kf = os.path.join(tmp, 'keys'); lf = os.path.join(tmp, 'lines')
    with open(kf, 'w') as f:
        for t in script:
            if t[0] == 'key': f.write('%d %d\n' % (t[1], t[2]))
    with open(lf, 'w', encoding='latin1') as f:
        for t in script:
            if t[0] == 'line': f.write(t[1] + '\n')
    env = dict(os.environ, ICE_KEYS=kf, ICE_LINES=lf)
    r = subprocess.run([exe], cwd=datadir or PORT_DATA, env=env, capture_output=True, timeout=120)
    out = r.stdout.decode('latin1')
    out = out.replace('\x1b[KEYS-EXHAUSTED]', '').replace('\x1b[LINES-EXHAUSTED]', '')
    s = Screen(); s.feed(out)
    return s, out, r.returncode

import re
def norm(s):
    """MS FORTRAN never prints the optional leading zero of F/E/D fields
    ('  .7174172', ' -.0474920870'); gfortran always does.  Map both outputs
    to the MS form so that only real differences are reported."""
    s = re.sub(r'(?<![0-9.])([-+])0\.(?=[0-9])', r' \1.', s)
    return re.sub(r'(?<![0-9.+\-])0\.(?=[0-9])', ' .', s)

def compare(script, label='', show=True, odata=None, pdata=None, clean=('E2E.OUT',)):
    for d in (odata or ORIG_DATA, pdata or PORT_DATA):
        for f in clean:
            if os.path.exists(os.path.join(d, f)): os.remove(os.path.join(d, f))
    so, to, mo = run_orig(script, odata)
    sp, tp, mp = run_port(script, pdata)
    a, b = norm(so.text()).split('\n'), norm(sp.text()).split('\n')
    bad = [i for i in range(25) if a[i] != b[i]]
    # also compare the whole output stream (catches differences that
    # scrolled off the final screen)
    strip = lambda s: re.sub(r'\x1b\[[^A-Za-z}\]]*[A-Za-z}\]]', '', norm(s)).replace('\r', '').split('\n')
    la, lb = strip(to), strip(tp)
    sbad = next((i for i in range(max(len(la), len(lb))) if i >= len(la) or i >= len(lb) or la[i] != lb[i]), None)
    tag = 'SAME' if not bad and sbad is None else 'DIFF'
    print('%-6s %-30s %s%s' % (tag, label, '' if not bad else 'rows %s' % bad,
          '' if sbad is None else ' stream line %d/%d,%d' % (sbad, len(la), len(lb))))
    if sbad is not None and not bad and show:
        for i in range(max(0, sbad - 1), min(sbad + 3, max(len(la), len(lb)))):
            print('  o %4d|%s' % (i, la[i] if i < len(la) else '<eof>'))
            print('  p %4d|%s' % (i, lb[i] if i < len(lb) else '<eof>'))
    if bad and show:
        for i in bad[:12]:
            print('  orig %2d|%s' % (i, a[i])); print('  port %2d|%s' % (i, b[i]))
    return not bad and sbad is None, so, sp, to, tp

if __name__ == '__main__':
    compare([], 'startup menu')
    compare([F(2), L('E2E.OUT'), ENTER], 'F2 create file')
    compare([F(2), L('E2E.OUT'), ENTER, F(2), ENTER], 'F2 create+close', clean=())
    compare([F(2), L('E2E.OUT'), ENTER, F(2), ENTER, F(2), L('E2E.OUT'), ENTER], 'F2 reopen', clean=())
    compare([F(10)], 'F10 quit')
