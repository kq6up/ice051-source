/* Resolve native data paths without changing the process working directory. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <limits.h>
#include <sys/stat.h>
#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

static char data_dir[PATH_MAX];

static int select_dir(const char *dir)
{
    char resolved[PATH_MAX], file[PATH_MAX];
    struct stat st;
    if (!realpath(dir, resolved)) return 0;
    int n = snprintf(file, sizeof file, "%s/PARAM.DAT", resolved);
    if (n < 0 || (size_t)n >= sizeof file || stat(file, &st) != 0 ||
        !S_ISREG(st.st_mode) || access(file, R_OK) != 0) return 0;
    strcpy(data_dir, resolved);
    return 1;
}

void iceinit_(void)
{
    const char *override = getenv("ICE_DATA_DIR");
    char exe[PATH_MAX], resolved[PATH_MAX], candidate[PATH_MAX];
    if (override && *override) {
        if (select_dir(override)) return;
        fprintf(stderr, "ICE: ICE_DATA_DIR '%s' does not contain a readable PARAM.DAT.\n", override);
        exit(1);
    }
    /* Retain explicit runs from a data directory, including verification data. */
    if (select_dir(".")) return;
#ifdef __APPLE__
    uint32_t size = sizeof exe;
    int have_exe = _NSGetExecutablePath(exe, &size) == 0;
#elif defined(__linux__)
    ssize_t n = readlink("/proc/self/exe", exe, sizeof exe - 1);
    int have_exe = n >= 0;
    if (have_exe) exe[n] = '\0';
#else
    int have_exe = 0;
#endif
    if (have_exe && realpath(exe, resolved)) {
        char *slash = strrchr(resolved, '/');
        if (slash) {
            *slash = '\0';
            if (select_dir(resolved)) return;
            /* root executable, build executable, or saved snapshot */
            const char *suffixes[] = { "/data", "/../data", "/../../data" };
            for (size_t i = 0; i < sizeof suffixes / sizeof *suffixes; i++) {
                int len = snprintf(candidate, sizeof candidate, "%s%s", resolved, suffixes[i]);
                if (len >= 0 && (size_t)len < sizeof candidate && select_dir(candidate)) return;
            }
        }
    }
    fprintf(stderr, "ICE: cannot find a readable PARAM.DAT. Set ICE_DATA_DIR to the converted data directory.\n");
    exit(1);
}

/* gfortran passes CHARACTER arguments followed by size_t string lengths. */
void icepath_(const char *name, char *result, size_t name_len, size_t result_len)
{
    while (name_len && name[name_len - 1] == ' ') name_len--;
    /* Explicit absolute or relative paths supplied for a user catalog stay put. */
    int explicit_path = memchr(name, '/', name_len) != NULL;
    size_t prefix = explicit_path ? 0 : strlen(data_dir);
    size_t required = prefix + (prefix ? 1 : 0) + name_len;
    if (required > result_len) {
        fprintf(stderr, "ICE: data path exceeds the Fortran filename buffer.\n");
        exit(1);
    }
    memset(result, ' ', result_len);
    if (prefix) { memcpy(result, data_dir, prefix); result[prefix++] = '/'; }
    memcpy(result + prefix, name, name_len);
}
