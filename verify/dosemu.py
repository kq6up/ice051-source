"""Minimal DOS + BIOS emulator (Unicorn) to run ICE.EXE headless.
Runs the native-x87 image (ICE_N.img) so no FP emulator interrupts are needed."""
import struct, json, os, sys, time
from unicorn import *
from unicorn.x86_const import *

HERE = os.path.dirname(os.path.abspath(__file__))
PSP = 0x0FF0
L = 0x1000
ENV = 0x0F00
REL = json.load(open(os.path.join(HERE, 'ICE_U.EXE.rel.json')))


class Halt(Exception):
    pass


class DOS:
    def __init__(self, image=None, datadir='.', keys=None, trace=False, maxins=None):
        self.img = image if image is not None else open(os.path.join(HERE, 'ICE_N.img'), 'rb').read()
        self.datadir = datadir
        self.keys = list(keys or [])      # list of (ascii, scan)
        self.files = {}
        self.nexth = 5
        self.screen = [[' '] * 80 for _ in range(25)]
        self.cur = [0, 0]
        self.transcript = []
        self.trace = trace
        self.exited = None
        self.heap = 0x8000                 # paragraph allocator for int21/48
        mu = self.mu = Uc(UC_ARCH_X86, UC_MODE_16)
        mu.mem_map(0, 0x100000)
        im = bytearray(self.img)
        for s, o in REL:
            a = s * 16 + o
            v = struct.unpack('<H', im[a:a + 2])[0]
            im[a:a + 2] = struct.pack('<H', (v + L) & 0xffff)
        mu.mem_write(L * 16, bytes(im))
        # PSP
        psp = bytearray(256)
        psp[0:2] = b'\xcd\x20'
        psp[2:4] = struct.pack('<H', 0x9FFF)
        psp[0x2c:0x2e] = struct.pack('<H', ENV)
        psp[0x80] = 0; psp[0x81] = 0x0d
        mu.mem_write(PSP * 16, bytes(psp))
        env = b'PATH=C:\\\x00COMSPEC=C:\\COMMAND.COM\x00\x00\x01\x00C:\\ICE.EXE\x00'
        mu.mem_write(ENV * 16, env)
        # BIOS data area: equipment word with coprocessor bit, video mode 3
        mu.mem_write(0x410, struct.pack('<H', 0x0023))
        mu.mem_write(0x449, b'\x03')
        mu.mem_write(0x44a, struct.pack('<H', 80))
        # IVT: point every vector at an IRET stub inside F000
        mu.mem_write(0xF0000, b'\xcf')
        for n in range(256):
            mu.mem_write(n * 4, struct.pack('<HH', 0, 0xF000))
        R = mu.reg_write
        R(UC_X86_REG_CS, L + 0x0ff5); R(UC_X86_REG_IP, 0x314c)
        R(UC_X86_REG_SS, L + 0x4412); R(UC_X86_REG_SP, 0x0800)
        R(UC_X86_REG_DS, PSP); R(UC_X86_REG_ES, PSP)
        mu.hook_add(UC_HOOK_INTR, self.on_int)
        self.ninst = 0
        if os.environ.get('ICE_QEMU_X87') != '1':
            import x87fix; x87fix.install(mu, L)

    # ---------------- helpers ----------------
    def r(self, reg): return self.mu.reg_read(reg)

    def w(self, reg, v): self.mu.reg_write(reg, v & 0xffff)

    def setcf(self, c):
        fl = self.r(UC_X86_REG_EFLAGS)
        fl = (fl | 1) if c else (fl & ~1)
        self.mu.reg_write(UC_X86_REG_EFLAGS, fl)

    def setzf(self, z):
        fl = self.r(UC_X86_REG_EFLAGS)
        fl = (fl | 0x40) if z else (fl & ~0x40)
        self.mu.reg_write(UC_X86_REG_EFLAGS, fl)

    def lin(self, seg, off): return (seg << 4) + off

    def asciiz(self, seg, off):
        b = self.mu.mem_read(self.lin(seg, off), 128)
        return bytes(b).split(b'\0')[0].decode('latin1')

    def hostpath(self, name):
        n = name.replace('\\', '/').split('/')[-1].split(':')[-1]
        for f in os.listdir(self.datadir):
            if f.upper() == n.upper(): return os.path.join(self.datadir, f)
        return os.path.join(self.datadir, n.upper())

    # ---------------- screen ----------------
    def putc(self, ch):
        self.transcript.append(ch)
        r, c = self.cur
        if ch == '\r': self.cur[1] = 0; return
        if ch == '\n':
            r += 1
        elif ch == '\b':
            self.cur[1] = max(0, c - 1); return
        elif ch == '\x07':
            return
        else:
            self.screen[r][c] = ch; c += 1
            if c >= 80: c = 0; r += 1
        if r >= 25:
            self.screen.pop(0); self.screen.append([' '] * 80); r = 24
        self.cur = [r, c]

    def text(self):
        return '\n'.join(''.join(row).rstrip() for row in self.screen)

    # ---------------- keyboard ----------------
    def nextkey(self):
        if not self.keys:
            raise Halt('out of keys')
        k = self.keys.pop(0)
        return k

    # ---------------- interrupts ----------------
    def on_int(self, mu, intno, user):
        try:
            if intno == 0x21: self.int21()
            elif intno == 0x10: self.int10()
            elif intno == 0x16: self.int16()
            elif intno == 0x11: self.w(UC_X86_REG_AX, 0x0023)
            elif intno == 0x12: self.w(UC_X86_REG_AX, 640)
            elif intno == 0x1a:
                t = int((time.time() % 86400) * 18.2065)
                self.w(UC_X86_REG_CX, t >> 16); self.w(UC_X86_REG_DX, t & 0xffff); self.w(UC_X86_REG_AX, 0)
            else:
                # vector through IVT if program installed a handler
                vec = struct.unpack('<HH', bytes(mu.mem_read(intno * 4, 4)))
                if vec != (0, 0xF000):
                    self.dispatch_ivt(intno, vec)
                else:
                    if self.trace: print('unhandled int %02x' % intno, file=sys.stderr)
        except Halt:
            raise
        except UcError:
            raise

    def dispatch_ivt(self, intno, vec):
        mu = self.mu
        ip = self.r(UC_X86_REG_IP); cs = self.r(UC_X86_REG_CS); fl = self.r(UC_X86_REG_EFLAGS)
        ss = self.r(UC_X86_REG_SS); sp = self.r(UC_X86_REG_SP)
        sp -= 6
        mu.mem_write(self.lin(ss, sp), struct.pack('<HHH', ip, cs, fl & 0xffff))
        self.w(UC_X86_REG_SP, sp)
        mu.reg_write(UC_X86_REG_CS, vec[1]); mu.reg_write(UC_X86_REG_IP, vec[0])

    def int10(self):
        ah = self.r(UC_X86_REG_AX) >> 8; al = self.r(UC_X86_REG_AX) & 0xff
        if ah == 0x0f: self.w(UC_X86_REG_AX, (80 << 8) | 3); self.w(UC_X86_REG_BX, 0)
        elif ah == 0x02:
            dx = self.r(UC_X86_REG_DX); self.cur = [min(24, dx >> 8), min(79, dx & 0xff)]
        elif ah == 0x03:
            self.w(UC_X86_REG_DX, (self.cur[0] << 8) | self.cur[1]); self.w(UC_X86_REG_CX, 0x0607)
        elif ah in (0x06, 0x07):
            cx = self.r(UC_X86_REG_CX); dx = self.r(UC_X86_REG_DX)
            r0, c0, r1, c1 = cx >> 8, cx & 0xff, dx >> 8, dx & 0xff
            n = al
            rows = list(range(r0, min(24, r1) + 1))
            if n == 0 or n > len(rows):
                for rr in rows:
                    for cc in range(c0, min(79, c1) + 1): self.screen[rr][cc] = ' '
            else:
                for _ in range(n):
                    if ah == 6:
                        for rr in rows[:-1]: self.screen[rr][c0:c1 + 1] = self.screen[rr + 1][c0:c1 + 1]
                        self.screen[rows[-1]][c0:c1 + 1] = [' '] * (c1 - c0 + 1)
                    else:
                        for rr in reversed(rows[1:]): self.screen[rr][c0:c1 + 1] = self.screen[rr - 1][c0:c1 + 1]
                        self.screen[rows[0]][c0:c1 + 1] = [' '] * (c1 - c0 + 1)
            self.transcript.append('\x0c')
        elif ah in (0x09, 0x0a):
            cx = self.r(UC_X86_REG_CX); r, c = self.cur
            for k in range(cx):
                if c + k < 80: self.screen[r][c + k] = chr(al)
            self.transcript.append(chr(al) * cx)
        elif ah == 0x0e: self.putc(chr(al))
        elif ah == 0x08: self.w(UC_X86_REG_AX, 0x0700 | ord(self.screen[self.cur[0]][self.cur[1]]))
        elif ah == 0x00: pass
        elif ah == 0x01: pass
        elif ah == 0x05: pass
        else:
            if self.trace: print('int10 ah=%02x' % ah, file=sys.stderr)

    def int16(self):
        ah = self.r(UC_X86_REG_AX) >> 8
        if ah in (0, 0x10):
            asc, scan = self.nextkey()
            self.w(UC_X86_REG_AX, (scan << 8) | asc)
        elif ah in (1, 0x11):
            if self.keys:
                asc, scan = self.keys[0]; self.w(UC_X86_REG_AX, (scan << 8) | asc); self.setzf(False)
            else:
                raise Halt('out of keys (poll)')
        elif ah == 2: self.w(UC_X86_REG_AX, 0)

    def int21(self):
        mu = self.mu
        ax = self.r(UC_X86_REG_AX); ah = ax >> 8; al = ax & 0xff
        bx = self.r(UC_X86_REG_BX); cx = self.r(UC_X86_REG_CX); dx = self.r(UC_X86_REG_DX)
        ds = self.r(UC_X86_REG_DS)
        if self.trace: print('int21 ah=%02x al=%02x bx=%04x cx=%04x' % (ah, al, bx, cx), file=sys.stderr)
        ok = lambda: self.setcf(False)
        err = lambda code: (self.w(UC_X86_REG_AX, code), self.setcf(True))
        if ah == 0x30: self.w(UC_X86_REG_AX, 0x1E03); self.w(UC_X86_REG_BX, 0); ok()
        elif ah == 0x4a: ok()
        elif ah == 0x48:
            seg = self.heap; self.heap += bx
            if self.heap > 0x9F00: err(8); self.w(UC_X86_REG_BX, 0x9F00 - seg); return
            self.w(UC_X86_REG_AX, seg); ok()
        elif ah == 0x49: ok()
        elif ah == 0x25: mu.mem_write(al * 4, struct.pack('<HH', dx, ds))
        elif ah == 0x35:
            off, seg = struct.unpack('<HH', bytes(mu.mem_read(al * 4, 4)))
            self.w(UC_X86_REG_BX, off); mu.reg_write(UC_X86_REG_ES, seg)
        elif ah in (0x3d, 0x3c, 0x5b):
            name = self.asciiz(ds, dx); p = self.hostpath(name)
            try:
                if ah == 0x3d:
                    mode = {0: 'rb', 1: 'r+b', 2: 'r+b'}.get(al & 3, 'rb')
                    f = open(p, mode)
                else:
                    f = open(p, 'w+b')
            except OSError:
                if self.trace: print('open fail', name, file=sys.stderr)
                err(2); return
            h = self.nexth; self.nexth += 1; self.files[h] = f
            self.w(UC_X86_REG_AX, h); ok()
        elif ah == 0x3e:
            f = self.files.pop(bx, None)
            if f: f.close()
            ok()
        elif ah == 0x3f:
            if bx == 0:
                # console line input
                data = b''
                while True:
                    asc, scan = self.nextkey()
                    if asc == 0x0d: data += b'\r\n'; self.putc('\r'); self.putc('\n'); break
                    if asc == 8:
                        data = data[:-1]; continue
                    if asc: data += bytes([asc]); self.putc(chr(asc))
                data = data[:cx]
            else:
                f = self.files.get(bx)
                if f is None: err(6); return
                data = f.read(cx)
            mu.mem_write(self.lin(ds, dx), data)
            self.w(UC_X86_REG_AX, len(data)); ok()
        elif ah == 0x40:
            data = bytes(mu.mem_read(self.lin(ds, dx), cx))
            if bx in (1, 2):
                for ch in data.decode('latin1'): self.putc(ch)
            elif bx == 4:
                pass
            else:
                f = self.files.get(bx)
                if f is None: err(6); return
                f.write(data)
            self.w(UC_X86_REG_AX, cx); ok()
        elif ah == 0x42:
            f = self.files.get(bx)
            if f is None: err(6); return
            off = (cx << 16) | dx
            if off & 0x80000000: off -= 1 << 32
            f.seek(off, al)
            pos = f.tell(); self.w(UC_X86_REG_AX, pos & 0xffff); self.w(UC_X86_REG_DX, pos >> 16); ok()
        elif ah == 0x44:
            if al == 0:
                self.w(UC_X86_REG_DX, 0x80D3 if bx < 3 else 0x0002); ok()
            else: ok()
        elif ah == 0x41:
            try: os.remove(self.hostpath(self.asciiz(ds, dx)))
            except OSError: pass
            ok()
        elif ah == 0x2c:
            t = time.localtime(); self.w(UC_X86_REG_CX, (t.tm_hour << 8) | t.tm_min); self.w(UC_X86_REG_DX, t.tm_sec << 8)
        elif ah == 0x2a:
            t = time.localtime(); self.w(UC_X86_REG_CX, t.tm_year); self.w(UC_X86_REG_DX, (t.tm_mon << 8) | t.tm_mday); self.w(UC_X86_REG_AX, t.tm_wday)
        elif ah == 0x0b: self.w(UC_X86_REG_AX, 0x0bff if self.keys else 0x0b00)
        elif ah == 0x4c:
            self.exited = al; raise Halt('exit %d' % al)
        elif ah in (0x19,): self.w(UC_X86_REG_AX, 2)
        elif ah == 0x4b: err(2)
        elif ah == 0x4d: self.w(UC_X86_REG_AX, 0)
        elif ah in (0x01, 0x07, 0x08):
            asc, scan = self.nextkey(); self.w(UC_X86_REG_AX, (ah << 8) | asc)
            if ah == 1: self.putc(chr(asc))
        elif ah == 0x02: self.putc(chr(dx & 0xff))
        elif ah == 0x06:
            if (dx & 0xff) == 0xff:
                if self.keys: asc, scan = self.nextkey(); self.w(UC_X86_REG_AX, 0x0600 | asc); self.setzf(False)
                else: raise Halt('out of keys (21/06)')
            else: self.putc(chr(dx & 0xff))
        elif ah == 0x09:
            s = bytes(mu.mem_read(self.lin(ds, dx), 2000)).split(b'$')[0].decode('latin1')
            for ch in s: self.putc(ch)
        elif ah == 0x24: pass
        elif ah == 0x43:
            p = self.hostpath(self.asciiz(ds, dx))
            if os.path.exists(p): self.w(UC_X86_REG_CX, 0x20); ok()
            else: err(2)
        else:
            if self.trace: print('int21 unhandled ah=%02x' % ah, file=sys.stderr)
            err(1)

    def run(self, max_count=200_000_000, until=None):
        try:
            pc = self.lin(self.r(UC_X86_REG_CS), self.r(UC_X86_REG_IP))
            self.mu.emu_start(pc, until if until else 0xFFFFF, count=max_count)
        except Halt as h:
            return str(h)
        except UcError as e:
            cs = self.r(UC_X86_REG_CS); ip = self.r(UC_X86_REG_IP)
            return 'UcError %s at %04x:%04x (img %05x)' % (e, cs, ip, (cs - L) * 16 + ip)
        return 'count exhausted'


# key helpers
def F(n):   # function key
    return (0, 0x3a + n) if n <= 10 else None


def typ(s):
    out = []
    for ch in s:
        if ch == '\n': out.append((13, 0x1c))
        else: out.append((ord(ch), 0))
    return out


ENTER = (13, 0x1c)

if __name__ == '__main__':
    d = DOS(keys=[(13, 0x1c)] + [F(7)] + typ('SUN\n') + [ENTER] + [F(10)], trace='-t' in sys.argv)
    print(d.run())
    print(d.text())
