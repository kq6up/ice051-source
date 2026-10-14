"""Live native executable regressions; build both variants before running."""
import os
import platform
import pty
import select
import shutil
import subprocess
import tempfile
import time
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SUN_KEYS = [59, 59, 60, 61, 65, 65, 68]
SUN_LINES = ["20261013.230000", "34", "-117", "SUN", ""]


class NativeRuntime(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="ice runtime ")
        self.cwd = Path(self.tmp.name)

    def tearDown(self):
        self.tmp.cleanup()

    def scripted(self, keys=SUN_KEYS, lines=SUN_LINES, env=None, exe=None):
        k = self.cwd / "keys"
        l = self.cwd / "lines"
        k.write_text("".join(f"0 {scan}\n" for scan in keys))
        l.write_text("".join(line + "\n" for line in lines))
        config = {k: v for k, v in os.environ.items() if not k.startswith("ICE_")}
        config.update(ICE_KEYS=str(k), ICE_LINES=str(l))
        config.update(env or {})
        return subprocess.run([str(exe or ROOT / "build/ice")], cwd=self.cwd,
                              env=config, capture_output=True, timeout=15)

    def test_sun_from_unrelated_directory(self):
        run = self.scripted()
        self.assertEqual(run.returncode, 0, run.stderr.decode(errors="replace"))
        out = run.stdout.decode("cp437")
        for expected in ["2026 Oct 13 at 23 00 00", "94.4 seconds", "168 28.0",
                         "s 8 03.3", "+25 39.6", "239.2"]:
            self.assertIn(expected, out)
        self.assertNotIn("not found", out)

    def test_explicit_data_and_catalog(self):
        # An override with spaces, and data reached independently of the executable.
        data = self.cwd / "selected data"
        shutil.copytree(ROOT / "data", data, ignore=shutil.ignore_patterns("ice"))
        run = self.scripted(keys=[67, 68], lines=["1", "N"],
                            env={"ICE_DATA_DIR": str(data)})
        self.assertEqual(run.returncode, 0, run.stderr.decode(errors="replace"))
        self.assertIn(b"Successfully opened catalog file", run.stdout)
        self.assertIn(b"200", run.stdout)

    def test_invalid_override_is_actionable(self):
        run = self.scripted(env={"ICE_DATA_DIR": str(self.cwd / "absent")})
        self.assertEqual(run.returncode, 1)
        self.assertIn(b"ICE_DATA_DIR", run.stderr)
        self.assertIn(b"readable PARAM.DAT", run.stderr)

    def test_relocated_executable_finds_its_own_data(self):
        bundle = self.cwd / "relocated ICE"
        bundle.mkdir()
        exe = bundle / "ice"
        shutil.copy2(ROOT / "build/ice", exe)
        if platform.system() == "Darwin":
            shutil.copy2(ROOT / "build/libiceconsole.dylib", bundle)
        shutil.copytree(ROOT / "data", bundle / "data",
                        ignore=shutil.ignore_patterns("ice", "*.dylib"))
        run = self.scripted(exe=exe)
        self.assertEqual(run.returncode, 0, run.stderr.decode(errors="replace"))
        self.assertIn(b"239.2", run.stdout)

    def test_file_output_stays_in_launch_directory_and_cp437(self):
        run = self.scripted(keys=[60] + SUN_KEYS[:-1] + [60, 68],
                            lines=["SIGHT.OUT", ""] + SUN_LINES + [""])
        self.assertEqual(run.returncode, 0, run.stderr.decode(errors="replace"))
        data = (self.cwd / "SIGHT.OUT").read_bytes()
        self.assertIn(b"239.2", data)
        self.assertIn(bytes([0xb3]), data)  # CP437 vertical separator
        self.assertNotIn("│".encode(), data)

    def test_corrected_model_is_separate(self):
        exe = ROOT / "build/ice-dt2026"
        self.assertTrue(exe.exists(), "Build with --deltat2026 first")
        run = self.scripted(exe=exe)
        self.assertEqual(run.returncode, 0, run.stderr.decode(errors="replace"))
        self.assertNotIn(b"94.4 seconds", run.stdout)
        self.assertIn(b"SUN", run.stdout)

    def test_launchers_from_unrelated_directory(self):
        for launcher in [ROOT / "run.sh", ROOT / "Start ICE.command"]:
            with self.subTest(launcher=launcher.name):
                run = self.scripted(exe=launcher)
                self.assertEqual(run.returncode, 0, run.stderr.decode(errors="replace"))
                self.assertIn(b"239.2", run.stdout)

    def test_live_terminal_unicode_and_function_keys(self):
        # No ICE_KEYS/ICE_LINES: exercise the actual terminal input and output.
        master, slave = pty.openpty()
        env = {k: v for k, v in os.environ.items() if not k.startswith("ICE_")}
        env["LC_ALL"] = "en_US.UTF-8"
        process = subprocess.Popen([str(ROOT / "build/ice")], cwd=self.cwd,
                                   env=env, stdin=slave, stdout=slave, stderr=slave)
        os.close(slave)
        transcript = bytearray()

        def until(marker):
            marker = marker.encode()
            pending = bytearray()
            deadline = time.monotonic() + 10
            while marker not in pending:
                if time.monotonic() >= deadline:
                    self.fail(f"Timed out waiting for {marker!r}: {pending!r}")
                if select.select([master], [], [], .2)[0]:
                    chunk = os.read(master, 65536)
                    pending.extend(chunk)
                    transcript.extend(chunk)

        try:
            until("Quit")
            os.write(master, b"\x1bOP")
            until("Accept values")
            for key, prompt, value in [(b"\x1bOP", "starting date", b"20261013.230000\n"),
                                       (b"\x1bOQ", "Enter latitude", b"34\n"),
                                       (b"\x1bOR", "Enter longitude", b"-117\n")]:
                os.write(master, key)
                until(prompt)
                os.write(master, value)
                until("Accept values")
            os.write(master, b"\x1b[18~")
            until("Quit")
            os.write(master, b"\x1b[18~")
            until("Enter object name")
            os.write(master, b"SUN\n")
            until("Hit [Enter]")
            display = transcript.decode("utf-8", errors="strict")
            self.assertIn("╔═", display)
            self.assertIn("│", display)
            self.assertIn("°", display)
            self.assertIn("239.2", display)
            self.assertNotIn("�", display)
            os.write(master, b"\n")
            until("Quit")
            os.write(master, b"\x1b[21~")
            self.assertEqual(process.wait(timeout=10), 0)
        finally:
            if process.poll() is None:
                process.terminate()
                process.wait()
            os.close(master)


if __name__ == "__main__":
    unittest.main()
