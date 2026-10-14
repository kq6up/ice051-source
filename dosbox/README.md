# DOS builds of ICE 0.51

This directory adds a separate DOSBox-X workflow. It does not change `build.sh`,
`src/`, or the preserved `dos/` sources used as inputs. Linux and macOS keep
their existing native build options.

Two rebuilt DOS executables are included:

| Executable | Model | October 13, 2026, 23:00 UTC |
|---|---|---|
| `bin/ice-vintage.exe` | Original 1988 Delta T | 94.4 seconds |
| `bin/ice-dt2026.exe` | Existing 2026 Delta T replacement | 69.2 seconds |

The historical reference executable remains in `reference/ice.zip`.
Each program requires the original DOS data alongside it. The preparation
script supplies this data and keeps each executable in its own directory.

## Run

Install DOSBox-X and make `dosbox-x` available on PATH. From the repository:

```sh
python3 dosbox/prepare.py
python3 dosbox/lab.py launch                 # rebuilt vintage model
python3 dosbox/lab.py launch --deltat2026     # updated 2026 model
python3 dosbox/lab.py launch --original      # historical reference
```

The Python scripts locate their files relative to themselves, so absolute
script paths work from any working directory. In DOS, F10 exits ICE, then
`EXIT` closes DOSBox-X. Defaults are set to the confirmed test date and time,
latitude 34 N and longitude 117 W. Press F7 and enter `SUN`.

To run without Python, copy one executable to a directory with all the DOS
data extracted from `reference/ice.zip`, rename it `ICE.EXE`, and run it with
that directory as the DOS working directory.

## Rebuild

Supply an installed Microsoft FORTRAN 4.10 toolchain in a separate folder
with `BIN` and `LIB` subdirectories. This needs `FL.EXE`, the compiler passes,
the distribution's linker and library manager, MASM 5.10, and `LLIBFORE.LIB`.
The compiler and its libraries are not included in this repository.

Before creating the drive, use:

```sh
python3 dosbox/prepare.py --fortran /path/to/FORTRAN
python3 dosbox/lab.py build
python3 dosbox/lab.py build --deltat2026
```

If the drive already exists, preparation refuses to overwrite it. Move it
aside first to preserve modified sources and settings. For an existing drive,
you can instead copy your installed `FORTRAN` folder into `dosbox/drive_c/`.

The original build compiles the preserved sources with `/c /AL /FPi /Od /4I4`:
large model, emulated coprocessor, no optimization, four-byte default integers.
Both builds use MASM for the screen helpers. The updated build selects
`DELTAT26.FOR` and changes only the NAVIG decimal-year day divisor from 30 to
365.25 in its generated source copy. The original sources remain intact.

## Validation

```sh
python3 dosbox/lab.py test
```

The three DOS executables completed the October 13 Sun test in DOSBox-X
2026.10.01 on Apple Silicon macOS. The rebuilt vintage output matched the
historical executable byte for byte (604 bytes). The updated build displayed
69.2 seconds. Exact exports and hashes are in `TEST_RESULTS.json`.
The user also confirmed the graphical macOS launcher works.

This checks that specific Sun calculation; it does not establish equivalence
for every menu or celestial body. `DELAY.COM` is a small test helper built
from the included `DELAY.ASM`; it lets queued synthetic keys drain before
DOSBox-X exits and does not alter either ICE executable.
