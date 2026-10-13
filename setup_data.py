"""Prepare a working directory for the gfortran build of ICE 0.51.

    python3 setup_data.py ICE_DIRECTORY DEST_DIRECTORY

PARAM.DAT and EPHnn.DAT are Microsoft FORTRAN unformatted sequential
files (a leading 'K' byte, records framed by length bytes, long records
split into 128-byte pieces); they are rewritten in gfortran's framing
(4-byte record length before and after each record).  The catalogs
(*.CAT, direct access, 68-byte records), ICE.DFT and everything else are
copied unchanged.  File names are upper-cased as the program expects."""
import os, struct, sys, shutil


def ms_records(data):
    if not data or data[0] != 0x4B:
        raise ValueError('not an MS FORTRAN unformatted file')
    i, recs, cur = 1, [], b''
    while i < len(data):
        n = data[i]; i += 1
        if n == 0x82:                      # end-of-file mark
            break
        k = 128 if n == 129 else n
        cur += data[i:i + k]; i += k
        if data[i] != n:
            raise ValueError('framing mismatch at byte %d' % i)
        i += 1
        if n != 129:
            recs.append(cur); cur = b''
    if cur:
        recs.append(cur)
    return recs


def main(src, dst):
    os.makedirs(dst, exist_ok=True)
    for name in sorted(os.listdir(src)):
        path = os.path.join(src, name)
        if not os.path.isfile(path):
            continue
        up = name.upper()
        out = os.path.join(dst, up)
        if up == 'PARAM.DAT' or (up.startswith('EPH') and up.endswith('.DAT')):
            recs = ms_records(open(path, 'rb').read())
            with open(out, 'wb') as f:
                for r in recs:
                    m = struct.pack('<i', len(r))
                    f.write(m + r + m)
            print('converted %-12s %4d records' % (up, len(recs)))
        else:
            shutil.copyfile(path, out)
            print('copied    %s' % up)


if __name__ == '__main__':
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    main(sys.argv[1], sys.argv[2])
