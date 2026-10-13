"""Make the Unicorn (QEMU) x87 transcendental instructions as precise as a
real coprocessor.  QEMU computes FYL2X, F2XM1, FPTAN and FPATAN through
host *double* functions, so e.g. the MS FORTRAN run-time's 10.D0**3 came
out as 1000.0000000000013 instead of the exact 1000.0 a real 8087/x87
delivers (checked on hardware with the same instruction sequence).
The instructions of the MS run-time (segment 0FF5) are intercepted and
evaluated in 80-bit extended precision with numpy.longdouble."""
import struct
import numpy as np
from unicorn.x86_const import *
from unicorn import UC_HOOK_CODE

LD = np.longdouble
TWO32 = LD(2) ** 32
LN2 = np.log(LD(2))
import os
DEBUG = os.environ.get('X87DEBUG') == '1'
STUB = 0xF0100               # F000:0100 scratch for push/pop stubs
# offsets in the run-time segment 0FF5
SITES = {0x1019: 'fyl2x', 0x102c: 'fyl2x', 0x10bf: 'f2xm1',
         0x1163: 'fptan', 0x11cf: 'fpatan'}


def to_ld(reg):
    mant, exp = reg
    sign = -1 if exp & 0x8000 else 1
    e = exp & 0x7fff
    if mant == 0 and e == 0:
        return LD(0) * sign
    v = LD(mant >> 32) * TWO32 + LD(mant & 0xffffffff)
    return sign * np.ldexp(v, e - 16383 - 63)


def from_ld(v):
    if v == 0:
        return (0, 0x8000 if np.signbit(v) else 0)
    sign = 0x8000 if v < 0 else 0
    m, e = np.frexp(abs(v))
    x = np.ldexp(m, 64)
    hi = np.floor(x / TWO32); lo = x - hi * TWO32
    return ((int(hi) << 32) | int(lo), sign | (int(e) + 16382))


def install(mu, L):
    seg = L + 0x0ff5

    def st(i):
        top = (mu.reg_read(UC_X86_REG_FPSW) >> 11) & 7
        return UC_X86_REG_FP0 + ((top + i) & 7)

    def resume(op, ip):
        # execute one helper op (push 1.0 / pop) then jump back.  Each site
        # has its own stub with fixed contents (Unicorn does not drop
        # translated blocks when memory is rewritten through the API).
        a = STUB + 16 * sorted(SITES).index(ip - 2)
        mu.mem_write(a, op + b'\xea' + struct.pack('<HH', ip, seg))
        mu.reg_write(UC_X86_REG_CS, a >> 4)
        mu.reg_write(UC_X86_REG_IP, a & 15)

    def hook(mu_, addr, size, kind):
        ip = (addr - seg * 16) + 2
        x = to_ld(mu.reg_read(st(0)))
        if DEBUG: print('x87', kind, repr(x), repr(to_ld(mu.reg_read(st(1)))), hex(mu.reg_read(UC_X86_REG_FPSW)))
        if kind == 'f2xm1':
            mu.reg_write(st(0), from_ld(np.expm1(x * LN2)))
            mu.reg_write(UC_X86_REG_IP, ip)
        elif kind == 'fyl2x':
            y = to_ld(mu.reg_read(st(1)))
            mu.reg_write(st(1), from_ld(y * np.log2(x)))
            resume(b'\xdd\xd8', ip)                  # fstp st(0)
        elif kind == 'fpatan':
            y = to_ld(mu.reg_read(st(1)))
            mu.reg_write(st(1), from_ld(np.arctan2(y, x)))
            resume(b'\xdd\xd8', ip)
        elif kind == 'fptan':
            mu.reg_write(st(0), from_ld(np.tan(x)))
            resume(b'\xd9\xe8', ip)                  # fld1

    for off, kind in SITES.items():
        a = (seg << 4) + off
        mu.hook_add(UC_HOOK_CODE, hook, kind, a, a)
