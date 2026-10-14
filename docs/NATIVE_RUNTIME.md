# Native terminal and data fixes

The current macOS build displays the DOS borders and degree signs as Unicode and finds the converted ephemeris data without requiring you to move the executable or change directories first.

## Start ICE

Double-click `Start ICE.command` in Finder. It opens ICE in Terminal and uses the project directory for any relative output filenames. After editing source, rebuild first:

```sh
./build.sh
./run.sh
```

`run.sh` and `build/ice` can be invoked by absolute path from any working directory. `run.sh` keeps that directory for relative output filenames. On macOS, keep `libiceconsole.dylib` beside the executable when moving a build; the copied executable in this checkout's `data/` can also load it from `../build/`.

The original and corrected Delta T models have separate binaries:

```sh
./build.sh                 # build/ice: original model
./build.sh --deltat2026    # build/ice-dt2026: corrected model
./run.sh --deltat2026
```

Building the corrected version no longer replaces the original executable. The existing corrected-year substitution is performed in a byte locale so macOS sed can process the DOS-encoded build copy.

## Choose a data folder

ICE first honors `ICE_DATA_DIR`. For example:

```sh
ICE_DATA_DIR="/path/to/converted data" ./run.sh
```

Without that setting, it checks the current directory for `PARAM.DAT`, then the real executable's directory, followed by `data/` below that directory or beside its parent or grandparent. This covers the normal repository layout, an executable copied into `data/`, and a relocated directory containing the executable and its own `data/` folder. Symbolic links resolve to the actual executable location.

Defaults, ephemeris coefficients, and catalog names are resolved against the selected data directory. A custom catalog name containing `/` is treated as an explicit path from the launch directory. Relative output filenames remain relative to the launch directory. The program does not change directories internally.

The selected directory must contain a readable `PARAM.DAT`. An invalid override or absent data directory produces a clear error with the override name, instead of starting the old disk-insertion loop. Use converted GNU Fortran data for native ICE; preserve the original DOS data for DOSBox-X.

## Console behavior

macOS binds libgfortran's I/O directly to its system library. The native console bridge is therefore linked as a small dylib, using dyld interposition to reach the existing CP437 conversion and console input handlers. Apple's [dyld source](https://github.com/opensource-apple/dyld/blob/master/src/ImageLoaderMachO.cpp) describes the interposing section used for this purpose. Linux retains the existing executable-based bridge.

Mac console output defaults to UTF-8. `ICE_CP437=1` retains raw DOS bytes when explicitly requested. Scripted verification with `ICE_KEYS` also retains raw bytes. Files written by Fortran remain in their original CP437 format; only terminal output is converted.

## Live validation on Apple Silicon macOS

Run the native regressions after building both variants:

```sh
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s verify -p test_native_runtime.py -v
```

Eight tests exercise the real executable, including interactive terminal input through a pseudo-terminal without scripted-key overrides. They check valid UTF-8 borders, separators, and degree symbols; the exact October 13, 2026, 23:00 UTC Sun test from an unrelated directory; an override with spaces; catalog loading; relocation of the executable and data; both launchers; separate Delta T builds; an actionable invalid-directory error; and CP437 file output in the launch directory.

The original-model Sun row remains GHA 168 28.0, declination S 8 03.3, Hc +25 39.6, Zn 239.2, Delta T 94.4 seconds. No astronomical formulas, constants, or Delta T routines were changed. Original DOS sources, reference packages, data, and baseline Mac snapshot remain preserved. The full DOSBox-X numerical comparison and Linux execution have not been rerun for this patch.
