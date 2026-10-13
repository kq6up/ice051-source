"""Build ICE.EXE (patched) from the unpacked original image + new DELTAT routine."""
import struct,json,sys,os
from capstone import *
here=os.path.dirname(os.path.abspath(__file__))
def native_to_emu(code):
    """Re-encode native 8087 ops (9B [26] D8-DF ...) and 'nop;wait' into the MS FORTRAN
    emulator INT 34h-3Dh form, exactly as the compiler/linker did for the rest of ICE."""
    b=bytearray(code); md=Cs(CS_ARCH_X86,CS_MODE_16); p=0; n=0
    while p<len(b):
        ins=next(md.disasm(bytes(b[p:p+16]),p),None)
        if ins is None: break
        if b[p]==0x9b and p+1<len(b) and 0xd8<=b[p+1]<=0xdf:
            esc=next(md.disasm(bytes(b[p+1:p+17]),0))
            b[p]=0xcd; b[p+1]=0x34+(b[p+1]-0xd8); n+=1; p+=1+esc.size; continue
        elif b[p]==0x9b and p+2<len(b) and b[p+1]==0x26 and 0xd8<=b[p+2]<=0xdf:
            esc=next(md.disasm(bytes(b[p+1:p+17]),0))
            b[p]=0xcd; b[p+1]=0x3c; n+=1; p+=1+esc.size; continue  # ES override: byte stays D8-DF
        elif b[p]==0x90 and p+1<len(b) and b[p+1]==0x9b and not (p+2<len(b) and 0xd8<=b[p+2]<=0xdf):
            b[p]=0xcd; b[p+1]=0x3d; n+=1; p+=2; continue
        elif b[p]==0x9b: raise SystemExit(f'unhandled wait at {p:x}')
        p+=ins.size
    return bytes(b),n
img=bytearray(open(os.path.join(here,'ICE_U.EXE.img'),'rb').read())
rel=json.load(open(os.path.join(here,'ICE_U.EXE.rel.json')))
DG=0x3825*16
SEG=0x1c*16
START,END=0x3480,0x3627                       # old DELTAT occupies 001C:3480..3626
new,n=native_to_emu(open(os.path.join(here,'deltat_new.bin'),'rb').read())
assert len(new)<=END-START, len(new)
assert not any(SEG+START<=s*16+o<SEG+END+2 for s,o in rel), 'relocation inside routine'
img[SEG+START:SEG+END]=new+b'\x90'*(END-START-len(new))
def putd(off,v): img[DG+off:DG+off+8]=struct.pack('<d',v)
assert struct.unpack('<d',img[DG+0x42e8:DG+0x42f0])[0]==30.0
putd(0x42e8,365.25)                            # caller: ID/30 -> ID/365.25
putd(0x43a0,1800.0+1.0/12.0+1.0/365.25)        # Y0
putd(0x43a8,0.5)
putd(0x43b0,0.01)
body=bytes(img)
open(os.path.join(here,'ICE_P.img'),'wb').write(body)
# MZ header (uncompressed): same entry/stack as the EXEPACK'd original
real_cs,real_ip,real_ss,real_sp=0x0ff5,0x314c,0x4412,0x0800
need=real_ss*16+real_sp
minalloc=max(0,(need-len(body)+15)//16)+1
rt=b''.join(struct.pack('<HH',o,s) for s,o in rel)
hpar=((28+len(rt)+15)//16+31)//32*32
total=hpar*16+len(body)
h=struct.pack('<2s13H',b'MZ',total%512,(total+511)//512,len(rel),hpar,minalloc,0xffff,real_ss,real_sp,0,real_ip,real_cs,28,0)+rt
h+=b'\0'*(hpar*16-len(h))
out=sys.argv[1] if len(sys.argv)>1 else os.path.join(here,'ICE_PATCHED.EXE')
open(out,'wb').write(h+body)
print('emu ops',n,'routine bytes',len(new),'minalloc',hex(minalloc),'->',out,len(h+body))
