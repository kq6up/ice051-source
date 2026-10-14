# ICE 0.51 macOS Handoff

Local source package: `/Users/chris/Downloads/ice051-source`.
Existing executable: `/Users/chris/Downloads/ice051-source/build/ice` (Mach-O arm64).

# Immediate Tasks

## Confirmed macOS Runtime Bug — F7 Celestial Navigation

The reconstructed ICE 0.51 program builds and launches on macOS, but selecting F7 (Celestial Navigation) produces the following error.

**Test conditions:**

- Date: October 13, 2026
- Time: 23:00:00 UTC
- Assumed longitude: 117°00.0′ W
- Assumed latitude: 34°00.0′ N
- ICE reports ΔT = 94.4 seconds (original model)

**Exact error message:**
```arduino
Celestial Navigation Data for 2026 Oct 13 at 23 00 00 UT (GMT)
                         Delta T = 94.4 seconds
For Assumed Location: Longitude W 117 00.0
                      Latitude  N  34 00.0

File PARAM.DAT not found on active disk.

Insert correct disk and press "Enter" when ready.
```

The error occurs after the celestial-navigation screen initializes, when ICE attempts to access its ephemeris data.

### Investigation priorities

1. Locate every reference to `PARAM.DAT` and the corresponding Fortran `OPEN` statements.
2. Determine the process working directory at execution time.
3. Verify that `PARAM.DAT` exists and is readable.
4. Examine the original DOS-to-GNU-Fortran data conversion process.
5. Determine whether ICE incorrectly reports an incompatible or unreadable file as a missing file.
6. Ensure the program can locate its data regardless of which directory the user launches it from, preferably through a configurable data directory or reliable launcher.
7. Repeat the exact F7 test after implementing the fix.

### Expected behavior

The program should read the ephemeris files and display the Sun, Moon, planets, and navigational-star calculations without a file error.

**Important:** Do not modify the ephemeris mathematics merely to fix a file-access problem. Preserve the original ΔT model and the corrected 2026 model as separate build options.

### Acceptance criterion

The exact October 13, 2026, 23:00 UTC test must complete successfully on Apple Silicon macOS, and its numerical output should be checked against the original DOS ICE executable running under DOSBox-X.

ICE successfully calculates and displays ΔT before encountering the `PARAM.DAT` error. That narrows the failure to a later part of the execution path, probably when the navigation calculation requests ephemeris coefficients. Investigate file paths and binary record compatibility first; the existing astronomical routines should not need to change to resolve this particular failure.

## Live reproduction findings — October 13, 2026

Codex ran the existing arm64 executable in a live terminal, without rebuilding or modifying the source, binary, or original data. Through F1, entered `20261013.230000`, latitude `34`, and longitude `-117`; accepted with F7, then selected main-menu F7 and entered `SUN`.

- Working directory `build/`: reproduced the exact missing-`PARAM.DAT` error after the date, location, and ΔT screen initialized.
- Working directory `data/`: the same test completed successfully and displayed the Sun and Aries rows.
- `data/PARAM.DAT` exists and is readable (4,122 bytes). Its first GNU Fortran record is 120 bytes with matching four-byte leading and trailing length markers. There is no `build/PARAM.DAT`.
- `src/ephem.for` lines 194–198 check `INQUIRE (FILE='PARAM.DAT', EXIST=LEX)` before opening the same relative filename. The observed missing-file message is produced by that existence check, before attempting a binary read.
- `setup_data.py` converts the original Microsoft Fortran sequential framing into GNU Fortran framing; the installed data worked in the live Sun calculation.

Observed successful output:

```text
Celestial Navigation Data for 2026 Oct 13 at 23 00 00 UT (GMT)
Delta T = 94.4 seconds
Longitude W 117 00.0; Latitude N 34 00.0

Object    GHA        Dec        Hc         Zn     Refr   SD    PA   Sum
SUN       168 28.0   S 8 03.3   +25 39.6   239.2  -2.1   16.1  0.1  14.1
ARIES       7 31.0
```

Immediate working launch command:

```sh
cd /Users/chris/Downloads/ice051-source/data
../build/ice
```

A reliable launcher or configurable data-directory implementation remains to be added. The exact Sun test passes with the correct working directory; Moon, planet, and full navigational-star tests and the original-DOS comparison under DOSBox-X remain outstanding. No ephemeris or ΔT mathematics were changed.


## Native runtime patch — October 13, 2026

The current source fixes the two native-runtime issues: macOS console conversion and automatic data discovery. The original failure and baseline results above describe the historical pre-patch executable. See `NATIVE_RUNTIME.md` for launch instructions, the data-directory override, and validation results. The original DOSBox-X numerical comparison remains outstanding.
