ICE Beta 0.51 (USNO Interactive Computer Ephemeris) - Delta T patch
====================================================================

What was found
  ICE.EXE is Microsoft FORTRAN 4.x code (1987 run-time), EXEPACK-compressed,
  using the MS 8087 emulator (INT 34h-3Dh).  Delta T lives in one subroutine
  at 001C:3480, called only from F7 Navigation (001C:20BB).  It is two
  polynomials in (year-1920): 1800-1920 (deg 6) and 1920-2050 (deg 10),
  fitted to data up to ~1988.  Extrapolated to 2026 it gives 94 s; the true
  value is 69.2 s.  The caller also forms the year as IY + IM/12 + ID/30,
  so the argument runs up to 2 years late in December.

What the patch changes  (ICE_DT26.EXE = drop-in replacement for ICE.EXE)
  * 001C:3480-3626  new DELTAT: 2-year Delta T table 1800-2050 (0.01 s units)
                    with linear interpolation, encoded in the same 8087-
                    emulator form the compiler used (works with or without
                    a coprocessor).  No relocations added.
  * DGROUP:42E8     30.0 -> 365.25  (caller now builds IY + IM/12 + ID/365.25)
  * DGROUP:43A0..   new constants Y0 = 1800+1/12+1/365.25, 0.5, 0.01
  * The EXE is stored unpacked (258 KB instead of 152 KB); memory use and
    entry point are unchanged.  Nothing else differs from the original.

Files
  DELTAT.FOR        recovered original routine + call site, and the
                    replacement routine in Fortran 77 (both verified against
                    the binary to <1e-9 s over 1795-2055)
  ICE_DISASM.ASM    full disassembly of all code segments, emulator INTs
                    turned back into native 8087 mnemonics (seg:offset)
  rebuild.sh        regenerates everything; re-run after
                    'pip install -U astropy-iers-data' to refresh the table
  unexepack.py      EXEPACK unpacker
  fpfix.py          emulator INT 34h-3Dh -> native 8087 (for disassembly)
  disasm_all.py     makes ICE_DISASM.ASM from fpfix output
  make_table.py     builds data/dt_grid.txt (sources documented inside)
  gen_asm.py        writes deltat_new.asm from the table
  build_patch.py    assembles the patched EXE
  unit_test.py      runs old/new DELTAT under Unicorn (needs unicorn)
  data/swe_dt.txt   yearly Delta T 1620-2028 from Swiss Ephemeris swephlib.c
  data/dt_grid.txt  the 126 table values actually used, with their source

Table sources
  1800-1972  Astronomical Almanac / Stephenson & Morrison (Swiss Ephemeris)
  1974-2026  IERS EOP: 32.184 + (TAI-UTC) - (UT1-UTC), 1 Jan values
  2028-2050  extrapolated: latest IERS value (Oct 2026, 69.21 s), slope of
             the Bulletin A prediction (+0.14 s/yr), plus long-term
             tidal curvature 0.0031 s/yr^2  -> 74.1 s in 2050.
             Treat post-2027 values as +/- several seconds.
