"""Unpack an EXEPACK-compressed DOS EXE.  usage: unexepack.py IN.EXE OUT.EXE
(writes OUT.EXE, OUT.EXE.img = load image, OUT.EXE.rel.json = relocations)"""
import struct,sys
d=open(sys.argv[1],'rb').read()
img=d[512:]
hdr=struct.unpack('<2s13H',d[:28]); cs=hdr[11]; img=d[hdr[4]*16:]
H=img[cs*16:cs*16+18]
real_ip,real_cs,mem_start,exe_size,real_sp,real_ss,dest_len,skip_len,sig=struct.unpack('<8H2s',H)
print(hex(real_ip),hex(real_cs),hex(real_sp),hex(real_ss),hex(dest_len),skip_len,sig)
comp=bytearray(img[:(cs-(skip_len-1))*16])
out=bytearray(dest_len*16)
src=len(comp)-1
while comp[src]==0xff: src-=1
dst=len(out)
while True:
    op=comp[src]; src-=1
    cnt=comp[src]<<8 | comp[src-1]; src-=2
    o=op&0xfe
    if o==0xb0:
        v=comp[src]; src-=1
        out[dst-cnt:dst]=bytes([v])*cnt; dst-=cnt
    elif o==0xb2:
        out[dst-cnt:dst]=comp[src-cnt+1:src+1]; src-=cnt; dst-=cnt
    else: raise SystemExit('bad op %x'%op)
    if op&1: break
print('remaining src',src,'dst',dst)
# prefix uncompressed bytes
out[:src+1]=comp[:src+1] if dst>src else out[:src+1]
# relocations
stub=img[cs*16:cs*16+exe_size]
p=0x12d
relocs=[]
for sec in range(16):
    n=struct.unpack('<H',stub[p:p+2])[0];p+=2
    for i in range(n):
        o=struct.unpack('<H',stub[p:p+2])[0];p+=2
        relocs.append((sec*0x1000,o))
print('relocs',len(relocs))
minalloc=(real_ss*16+real_sp-len(out)+15)//16+1
# build MZ
body=bytes(out)
hdr_rel=b''.join(struct.pack('<HH',o,s) for s,o in relocs)
hsize=28+len(hdr_rel); hpar=(hsize+15)//16; hpar=(hpar+31)//32*32
total=hpar*16+len(body)
h=struct.pack('<2s13H',b'MZ',total%512,(total+511)//512,len(relocs),hpar,minalloc,0xffff,real_ss,real_sp,0,real_ip,real_cs,28,0)
h=h+hdr_rel; h+=b'\0'*(hpar*16-len(h))
open(sys.argv[2],'wb').write(h+body)
open(sys.argv[2]+'.img','wb').write(body)
import json;json.dump(relocs,open(sys.argv[2]+'.rel.json','w'))
