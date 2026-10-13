"""Prepare the reference: unpack the original ICE.EXE (EXEPACK) and turn
its 8087-emulator calls (INT 34h..3Dh) into native x87 instructions so
that it can run in the Unicorn based DOS emulator (dosemu.py).

    python3 prepare.py /path/to/ICE.EXE

Writes ICE_U.img (load image), ICE_U.EXE.rel.json (relocations) and
ICE_N.img (native x87 image) next to this script and checks them
against the version used for the recovery (ICE 0.51, 152 KB EXE)."""
import hashlib, json, os, struct, sys
from capstone import Cs, CS_ARCH_X86, CS_MODE_16

HERE = os.path.dirname(os.path.abspath(__file__))
SHA_EXE = '644eabb1cd9e14256feb23c356d37bb995fe5866c8db85145e310168ee750ffe'
SHA_NAT = '224b5d0a3cc21c4401b218a749d04a91fa2beddfab322707ff60bd0511a754c0'
CODE_END = 0x1C440          # code segments end where the far data begins


def unexepack(d):
    hdr = struct.unpack('<2s13H', d[:28])
    cs = hdr[11]
    img = d[hdr[4] * 16:]
    (real_ip, real_cs, mem_start, exe_size, real_sp, real_ss, dest_len,
     skip_len, sig) = struct.unpack('<8H2s', img[cs * 16:cs * 16 + 18])
    comp = bytearray(img[:(cs - (skip_len - 1)) * 16])
    out = bytearray(dest_len * 16)
    src = len(comp) - 1
    while comp[src] == 0xff:
        src -= 1
    dst = len(out)
    while True:
        op = comp[src]; src -= 1
        cnt = comp[src] << 8 | comp[src - 1]; src -= 2
        if op & 0xfe == 0xb0:
            out[dst - cnt:dst] = bytes([comp[src]]) * cnt; src -= 1; dst -= cnt
        elif op & 0xfe == 0xb2:
            out[dst - cnt:dst] = comp[src - cnt + 1:src + 1]; src -= cnt; dst -= cnt
        else:
            raise SystemExit('not an EXEPACK file I know (op %x)' % op)
        if op & 1:
            break
    if dst > src:
        out[:src + 1] = comp[:src + 1]
    stub = img[cs * 16:cs * 16 + exe_size]
    p, relocs = 0x12d, []
    for sec in range(16):
        n = struct.unpack('<H', stub[p:p + 2])[0]; p += 2
        for _ in range(n):
            relocs.append((sec * 0x1000, struct.unpack('<H', stub[p:p + 2])[0])); p += 2
    return bytes(out), relocs


def native(b):
    """INT 34h..3Bh -> FWAIT + ESC D8..DF; INT 3Ch -> FWAIT + segment
    override (+ ESC); INT 3Dh -> NOP FWAIT"""
    b = bytearray(b)
    seg = {0xc0: 0x26, 0x80: 0x2e, 0x40: 0x36, 0x00: 0x3e}
    md = Cs(CS_ARCH_X86, CS_MODE_16)
    p = 0
    while p < CODE_END:
        if b[p] == 0xcd and 0x34 <= b[p + 1] <= 0x3d:
            k = b[p + 1]
            if k <= 0x3b:
                b[p] = 0x9b; b[p + 1] = 0xd8 + (k - 0x34)
            elif k == 0x3c:
                x = b[p + 2]
                b[p] = 0x9b; b[p + 1] = seg[x & 0xc0]; b[p + 2] = (x & 0x3f) | 0xc0
            else:
                b[p] = 0x90; b[p + 1] = 0x9b
        ins = next(md.disasm(bytes(b[p:p + 16]), 0), None)
        p += ins.size if ins else 1
    return bytes(b)


def main(exe):
    d = open(exe, 'rb').read()
    if hashlib.sha256(d).hexdigest() != SHA_EXE:
        print('warning: this ICE.EXE differs from the one the source was recovered from')
    img, rel = unexepack(d)
    nat = native(img)
    open(os.path.join(HERE, 'ICE_U.img'), 'wb').write(img)
    json.dump(rel, open(os.path.join(HERE, 'ICE_U.EXE.rel.json'), 'w'))
    open(os.path.join(HERE, 'ICE_N.img'), 'wb').write(nat)
    ok = hashlib.sha256(nat).hexdigest() == SHA_NAT
    print('ICE_N.img written,', 'identical to the reference image' if ok else 'DIFFERENT from the reference image')


if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
