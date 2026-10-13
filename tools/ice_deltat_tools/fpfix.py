# convert MS FORTRAN x87 emulator INT 34h-3Dh sequences back to native 8087 opcodes
import sys
from capstone import *
def fix(b, start, end):
    b=bytearray(b)
    md=Cs(CS_ARCH_X86,CS_MODE_16)
    p=start
    n=0
    while p<end:
        if b[p]==0xcd and 0x34<=b[p+1]<=0x3d:
            k=b[p+1]
            if k<=0x3b: b[p]=0x9b; b[p+1]=0xd8+(k-0x34)
            elif k==0x3c:
                x=b[p+2]; top=x&0xc0
                seg={0xc0:0x26,0x80:0x3e,0x40:0x36,0x00:0x2e}[top]
                # DS stored as top=0x80? derive: DS adds 0x40 -> xx=0x98..; SS adds 0x80 -> 0x58..; CS adds 0xC0 -> 0x18..; ES none -> 0xD8
                seg={0xc0:0x26,0x80:0x3e,0x40:0x36,0x00:0x2e}[top]
                b[p]=0x9b; b[p+1]=seg; b[p+2]=(x&0x3f)|0xc0
            elif k==0x3d: b[p]=0x90; b[p+1]=0x9b
            n+=1
        ins=next(md.disasm(bytes(b[p:p+16]),0),None)
        p+= ins.size if ins else 1
    return bytes(b),n
if __name__=='__main__':
    b=open(sys.argv[1],'rb').read()
    s=int(sys.argv[3],0); e=int(sys.argv[4],0)
    nb,n=fix(b,s,e)
    open(sys.argv[2],'wb').write(nb); print('fixed',n)
