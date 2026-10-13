import struct
from unicorn import *
from unicorn.x86_const import *
L=0x1000  # load segment
def call_deltat(img,year):
    mu=Uc(UC_ARCH_X86,UC_MODE_16)
    mu.mem_map(0,0x100000)
    mu.mem_write(L*16,img)
    DS=L+0x3825
    # args in a scratch area
    A=0x8000*16
    mu.mem_write(A,struct.pack('<d',year)); mu.mem_write(A+8,struct.pack('<d',-999.0))
    SS=0x9000; sp=0xfff0
    stack=struct.pack('<HHHHHH',0xfff0,0x7000, 8,0x8000, 0,0x8000)  # ret ip,cs, DT far, YEAR far (pascal: YEAR pushed first)
    sp-=len(stack); mu.mem_write(SS*16+sp,stack)
    mu.mem_write(0x7000*16+0xfff0,b'\xf4')
    for r,v in ((UC_X86_REG_SS,SS),(UC_X86_REG_SP,sp),(UC_X86_REG_DS,DS),(UC_X86_REG_ES,DS),(UC_X86_REG_CS,L+0x1c)):
        mu.reg_write(r,v)
    mu.emu_start(((L+0x1c)<<4)+0x3480, 0x7000*16+0xfff0, count=5000)
    assert mu.reg_read(UC_X86_REG_SP)==0xfff0, hex(mu.reg_read(UC_X86_REG_SP))
    return struct.unpack('<d',mu.mem_read(A+8,8))[0]
old=open('ICE_N.img','rb').read(); new=open('ICE_PN.img','rb').read()
grid={int(l.split()[0]):float(l.split()[1]) for l in open('data/dt_grid.txt') if not l.startswith('#')}
swe={int(a):float(b) for a,b in (l.split() for l in open('data/swe_dt.txt'))}
def ref(y):
    y0=1800+int((y-1800)//2)*2; y0=min(max(y0,1800),2048)
    return grid[y0]+(grid[y0+2]-grid[y0])*(y-y0)/2
print(' date         old ICE   patched   reference(yearly AA/IERS where available)')
for (Y,M,D) in [(1801,1,1),(1820,6,15),(1870,1,1),(1900,1,1),(1920,1,1),(1950,1,1),(1988,10,1),(2000,1,1),(2010,1,1),(2020,1,1),(2024,1,1),(2026,1,1),(2026,10,12),(2030,1,1),(2040,1,1),(2049,6,1)]:
    yo=Y+M/12+D/30; yn=Y+M/12+D/365.25
    t=Y+(M-1)/12+(D-1)/365.25
    r = swe[Y] if (M,D)==(1,1) and Y<=2026 else ref(t)
    if Y>=1974 and (M,D)==(1,1): r=None
    o=call_deltat(old,yo); n=call_deltat(new,yn)
    print(f'{Y:4d}-{M:02d}-{D:02d}  {o:8.2f}  {n:8.2f}   {"" if r is None else f"{r:8.2f}"}')
