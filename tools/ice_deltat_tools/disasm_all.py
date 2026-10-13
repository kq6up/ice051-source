import sys,bisect
from capstone import *
b=open('ICE_N.img','rb').read()
segs=[0x13,0x1c,0x37e,0x6b6,0x917,0xa09,0xc4d,0xff5,0x1367,0x15ba,0x15c4,0x1aad]
md=Cs(CS_ARCH_X86,CS_MODE_16)
out=open('all.asm','w')
for i,s in enumerate(segs):
    st=s*16; en=segs[i+1]*16 if i+1<len(segs) else 0x1ab20
    out.write(f'\n;===== SEGMENT {s:04x} ({st:05x}-{en:05x})\n')
    p=st
    while p<en:
        ins=next(md.disasm(b[p:p+16],p-st),None)
        if not ins:
            out.write(f'{s:04x}:{p-st:04x}  db {b[p]:02x}\n'); p+=1; continue
        out.write(f'{s:04x}:{p-st:04x}  {ins.bytes.hex():<16} {ins.mnemonic} {ins.op_str}\n')
        p+=ins.size
