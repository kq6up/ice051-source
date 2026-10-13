# October 13, 2026 baseline

The `baseline-2026-10-13` tag preserves the source and data as they were when the existing Apple Silicon binary completed the F7 Sun test. No astronomical routines or display code were changed for publication.

## Included material

- `src/`, `dos/`, build scripts, and `verify/`: recovered source and supplied verification tools.
- `data/`: installed GNU Fortran ephemeris records, catalogs, and supplied DOS programs/documentation.
- `snapshots/macos-arm64/ice`: byte-for-byte copy of the working Mac binary from `data/ice`.
- `reference/ice.zip`: original DOS package, including the original unconverted ephemeris data.
- `reference/ice051-source.zip`: supplied reconstructed source archive.
- `reference/ice_deltat_tools.zip`, `tools/ice_deltat_tools/`, and `reference/ICE_DT26.EXE`: supplied corrected-Delta-T tools and DOS binary, preserved without edits.
- `docs/ICE_HANDOFF.md`: exact F7 reproduction and live macOS findings.

`build/` is generated and ignored. The user's copied `data/ice` remains in place and is ignored; its baseline copy is tracked in `snapshots/`.

## Run on this Mac

From the repository root:

```sh
cd data
../snapshots/macos-arm64/ice
```

The preserved Mac binary uses the locally installed GNU Fortran libraries. On a different system, rebuild using `./build.sh` and run `../build/ice` from `data/`. The build's `--deltat2026` option remains separate from the original model. The data in `data/` is already converted; use the original DOS data extracted from `reference/ice.zip` as the input when testing `setup_data.py`.

## Validation status

Live Apple Silicon terminal test: October 13, 2026, 23:00 UTC; 34 N, 117 W; F7, SUN. Original model: Delta T 94.4 seconds, Sun GHA 168 28.0, declination S 8 03.3, Hc +25 39.6, Zn 239.2. It succeeds from `data/` and reproduces the missing PARAM.DAT message from `build/`.

DOS border characters still display incorrectly on macOS; that issue is deferred at the user's request. A complete DOSBox-X numerical comparison and the full Moon/planet/star test remain unverified. The supplied README's broader validation claims have not been independently rerun here.

## Track and undo changes

Commit each deliberate change with `git add` and `git commit`, then `git push`. Use `git log --oneline` to see checkpoints. To undo a published commit while preserving history, use `git revert <commit-sha>`, then push the new revert commit.

To inspect or run the original baseline in a separate directory without disturbing current work:

```sh
git worktree add --detach ../ice051-baseline baseline-2026-10-13
```
