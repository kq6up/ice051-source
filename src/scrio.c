/* scrio.c -- portable stand-ins for ICE's assembler helpers (segment
   0013 of ICE.EXE, see scrio.asm), callable from gfortran, plus the bits
   of the MS FORTRAN console behaviour that gfortran does not have.
   The original routines are BIOS calls:
     SETCUR (ICOL, IROW)          INT 10h AH=02h   (byte arguments)
     GETKEY (IASC, ISCAN)         INT 16h AH=00h   (ASCII, scan code)
     CLRWIN (IL, IT, IR, IB)      INT 10h AH=06h AL=0 attr 0Fh, then cursor
                                  to (IT, IL)
     CUROFF / CURON               INT 10h AH=01h  (cursor shape 20h / 0007h)
     DOSXIT                       INT 21h AX=4C00h
   Here the screen side is emitted as ANSI escape sequences on stdout.
   Keys are read from the terminal (function keys F1..F10 and Alt-F1 are
   decoded from the usual xterm / Linux console sequences) or, for
   scripted testing, from the file named by $ICE_KEYS (one "ascii scan"
   pair per line); $ICE_LINES then names a file of input lines that
   replaces stdin and is echoed, as DOS does.
   The program writes code page 437 (box drawing, the degree sign); on a
   UTF-8 terminal the screen output is converted, unless $ICE_CP437 is
   set.  Files written by the program stay in code page 437.          */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <termios.h>
#include <sys/syscall.h>

/* Mach-O uses two-level symbol binding: libgfortran calls libSystem
   directly, so an executable symbol named write/read cannot override it.
   Use dyld interposition below for calls made by the runtime. */
#ifdef __APPLE__
#define write ice_console_write
#define read ice_console_read
#endif

/* ------------------------------------------------------------------ */
/* screen output, optionally code page 437 -> UTF-8                     */
static const unsigned short cp437[128] = {
    0x00C7,0x00FC,0x00E9,0x00E2,0x00E4,0x00E0,0x00E5,0x00E7,0x00EA,0x00EB,
    0x00E8,0x00EF,0x00EE,0x00EC,0x00C4,0x00C5,0x00C9,0x00E6,0x00C6,0x00F4,
    0x00F6,0x00F2,0x00FB,0x00F9,0x00FF,0x00D6,0x00DC,0x00A2,0x00A3,0x00A5,
    0x20A7,0x0192,0x00E1,0x00ED,0x00F3,0x00FA,0x00F1,0x00D1,0x00AA,0x00BA,
    0x00BF,0x2310,0x00AC,0x00BD,0x00BC,0x00A1,0x00AB,0x00BB,0x2591,0x2592,
    0x2593,0x2502,0x2524,0x2561,0x2562,0x2556,0x2555,0x2563,0x2551,0x2557,
    0x255D,0x255C,0x255B,0x2510,0x2514,0x2534,0x252C,0x251C,0x2500,0x253C,
    0x255E,0x255F,0x255A,0x2554,0x2569,0x2566,0x2560,0x2550,0x256C,0x2567,
    0x2568,0x2564,0x2565,0x2559,0x2558,0x2552,0x2553,0x256B,0x256A,0x2518,
    0x250C,0x2588,0x2584,0x258C,0x2590,0x2580,0x03B1,0x00DF,0x0393,0x03C0,
    0x03A3,0x03C3,0x00B5,0x03C4,0x03A6,0x0398,0x03A9,0x03B4,0x221E,0x03C6,
    0x03B5,0x2229,0x2261,0x00B1,0x2265,0x2264,0x2320,0x2321,0x00F7,0x2248,
    0x00B0,0x2219,0x00B7,0x221A,0x207F,0x00B2,0x25A0,0x00A0 };
static int utf8 = -1;
static void decide(void)
{
    const char *v;
    utf8 = 0;
#ifdef __APPLE__
    utf8 = 1;  /* macOS terminals use Unicode even without locale variables */
#endif
    if (getenv("ICE_CP437") || getenv("ICE_KEYS")) { utf8 = 0; return; }
    if (!(v = getenv("LC_ALL")) || !*v)
        if (!(v = getenv("LC_CTYPE")) || !*v) v = getenv("LANG");
    if (v) {
        char b[64]; size_t i;
        for (i = 0; v[i] && i < sizeof b - 1; i++) b[i] = toupper((unsigned char) v[i]);
        b[i] = 0;
        if (strstr(b, "UTF-8") || strstr(b, "UTF8")) utf8 = 1;
    }
}
static void raw(const char *s, size_t n)
{
    if (utf8 < 0) decide();
    if (!utf8) { syscall(SYS_write, 1, s, n); return; }
    char b[1536]; size_t k = 0, i;
    for (i = 0; i < n; i++) {
        unsigned char c = s[i];
        if (c < 0x80) b[k++] = c;
        else {
            unsigned u = cp437[c - 0x80];
            if (u < 0x800) { b[k++] = 0xC0 | (u >> 6); b[k++] = 0x80 | (u & 0x3F); }
            else { b[k++] = 0xE0 | (u >> 12); b[k++] = 0x80 | ((u >> 6) & 0x3F);
                   b[k++] = 0x80 | (u & 0x3F); }
        }
        if (k > sizeof b - 4) { syscall(SYS_write, 1, b, k); k = 0; }
    }
    if (k) syscall(SYS_write, 1, b, k);
}
static void out(const char *s) { raw(s, strlen(s)); }
extern void _gfortran_flush_i4(int *);
static void flush6(void) { int u = 6; _gfortran_flush_i4(&u); }

/* MS FORTRAN treats the console as a carriage-control device: the first
   character of every record is a control character (blank = new line
   before the record) and nothing is written after it.  After a READ from
   the console the line is already fresh and no new line is started.
   A console READ first ends a pending record with CR LF (unless it was
   written with $).  gfortran writes "text\n" per record, so translate
   here.                                                               */
static int at_rec = 1, fresh = 1;   /* the screen starts at a fresh line */
static void newline(void) { if (!fresh) raw("\r\n", 2); fresh = 0; }
ssize_t write(int fd, const void *buf, size_t n)
{
    if (fd != 1) return syscall(SYS_write, fd, buf, n);
    const unsigned char *p = buf;
    size_t i, start = 0;
    for (i = 0; i < n; i++) {
        unsigned char c = p[i];
        if (at_rec) {
            if (i > start) raw((const char *)p + start, i - start);
            at_rec = 0;
            if (c == '+') { raw("\r", 1); fresh = 0; }
            else if (c == '0') { newline(); raw("\r\n", 2); }
            else newline();
            if (c == '\n') at_rec = 1;
            start = i + 1;
        } else if (c == '\n') {
            if (i > start) raw((const char *)p + start, i - start);
            at_rec = 1; start = i + 1;
        }
    }
    if (n > start) raw((const char *)p + start, n - start);
    return n;
}

/* ------------------------------------------------------------------ */
void setcur_(signed char *icol, signed char *irow)
{
    flush6();
    char b[32];
    snprintf(b, sizeof b, "\033[%d;%dH", *irow + 1, *icol + 1);
    out(b);
}

void clrwin_(signed char *il, signed char *it, signed char *ir, signed char *ib)
{
    flush6();
    char b[48];
    snprintf(b, sizeof b, "\033[%d;%d;%d;%d}", *it + 1, *il + 1, *ib + 1, *ir + 1);
    out(b);
}

void curoff_(void) { out("\033[?25l"); }
void curon_(void)  { out("\033[?25h"); }
void dosxit_(void) { exit(0); }

/* ------------------------------------------------------------------ */
/* keyboard                                                             */
static int rdbyte(int timeout_ds)          /* -1 on timeout / EOF */
{
    struct termios t;
    unsigned char c;
    tcgetattr(0, &t);
    t.c_cc[VMIN] = timeout_ds ? 0 : 1;
    t.c_cc[VTIME] = timeout_ds;
    tcsetattr(0, TCSANOW, &t);
    return syscall(SYS_read, 0, &c, 1) == 1 ? c : -1;
}

static void ttykey(int *a, int *s)
{
    struct termios old, t;
    int c;
    if (tcgetattr(0, &old) != 0) {        /* not a terminal: plain bytes */
        unsigned char b;
        if (syscall(SYS_read, 0, &b, 1) != 1) exit(0);
        *a = b; *s = b == 13 || b == 10 ? 0x1c : 0;
        return;
    }
    t = old;
    t.c_lflag &= ~(ICANON | ECHO);
    tcsetattr(0, TCSANOW, &t);
    for (;;) {
        *a = 0; *s = 0;
        c = rdbyte(0);
        if (c < 0) { tcsetattr(0, TCSANOW, &old); exit(0); }
        if (c != 27) {
            if (c == 10) c = 13;
            *a = c; *s = c == 13 ? 0x1c : 0;
            break;
        }
        /* escape sequence: ESC O P..S, ESC [ n ~, ESC [ [ A..E, ESC [1;3P */
        char seq[16]; int n = 0;
        while (n < 15 && (c = rdbyte(1)) >= 0) {
            seq[n++] = c;
            if (n > 1 && (isalpha(c) || c == '~')) break;
        }
        seq[n] = 0;
        if (n == 2 && seq[0] == 'O' && seq[1] >= 'P' && seq[1] <= 'S')
            *s = 59 + seq[1] - 'P';
        else if (n == 3 && seq[0] == '[' && seq[1] == '[' && seq[2] >= 'A' && seq[2] <= 'E')
            *s = 59 + seq[2] - 'A';
        else if (!strcmp(seq, "[1;3P") || !strcmp(seq, "[11;3~"))
            *s = 104;                     /* Alt-F1 */
        else if (n > 1 && seq[0] == '[' && seq[n - 1] == '~') {
            static const int code[] = { 11, 12, 13, 14, 15, 17, 18, 19, 20, 21 };
            int k, v = atoi(seq + 1);
            for (k = 0; k < 10; k++) if (code[k] == v) *s = 59 + k;
        }
        if (*s) break;
    }
    tcsetattr(0, TCSANOW, &old);
}

static FILE *keys;
void getkey_(short *iasc, short *iscan)
{
    int a = 0, s = 0;
    flush6();
    if (!keys) {
        const char *k = getenv("ICE_KEYS");
        if (!k) { ttykey(&a, &s); *iasc = a; *iscan = s; return; }
        keys = fopen(k, "r");
    }
    if (!keys || fscanf(keys, "%d %d", &a, &s) != 2) {
        out("\033[KEYS-EXHAUSTED]"); exit(3);
    }
    *iasc = a; *iscan = s;
}

/* ------------------------------------------------------------------ */
/* line input: scripted lines with DOS-style echo when $ICE_LINES is set */
static FILE *lines;
ssize_t read(int fd, void *buf, size_t n)
{
    if (fd == 0) {
        flush6();
        /* a READ terminates a pending output record (one not ended by $) */
        if (at_rec && !fresh) raw("\r\n", 2);
        at_rec = 1;
    }
    if (fd == 0 && getenv("ICE_LINES")) {
        char line[512];
        if (!lines) lines = fopen(getenv("ICE_LINES"), "r");
        if (!lines || !fgets(line, sizeof line, lines)) { out("\033[LINES-EXHAUSTED]"); exit(4); }
        size_t l = strlen(line);
        if (l > n) l = n;
        memcpy(buf, line, l);
        if (l && line[l-1] == '\n') { raw(line, l - 1); raw("\r\n", 2); }
        else raw(line, l);
        fresh = 1;
        return l;
    }
    ssize_t r = syscall(SYS_read, fd, buf, n);
    if (fd == 0) fresh = 1;
    return r;
}

/* MS FORTRAN PAUSE: prints the message, waits for a line */
void mspaus_(char *msg, int len)
{
    char b[256];
    if (len > 200) len = 200;
    memcpy(b, msg, len); b[len] = 0;
    flush6();
    out("\r\n"); out(b); out("\r\n");
    at_rec = 1; fresh = 1;
    char line[512];
    ssize_t r = read(0, line, sizeof line);
    (void) r;
    fresh = 0;
}

#ifdef __APPLE__
#undef write
#undef read
#define ICE_INTERPOSE(replacement, original) \
    static const struct { const void *new_fn; const void *old_fn; } \
    ice_interpose_##original __attribute__((used, section("__DATA,__interpose,interposing"))) = \
        { (const void *)&replacement, (const void *)&original }
ICE_INTERPOSE(ice_console_write, write);
ICE_INTERPOSE(ice_console_read, read);
#endif
