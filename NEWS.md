# cts 1.0-25

## CRAN Check Fixes

### Compilation & Portability
* **Strict-Prototypes Compliance** (Windows gcc-ASAN): Updated C function declarations in `src/cts_init.c` to use strict standard prototypes by replacing empty parameter lists `()` with `(void)` for all Fortran routine declarations (`complete`, `cspec`, `display`, `forecast`, `kfilsm`, `loop`, `setcom`, `setfor`, `setkfilsm`, `setup`, `setupdate`, `update`). This resolves `-Wstrict-prototypes` warnings.

* **Symbol Registration and Dynamic Lookup**: Properly configured `R_registerRoutines()` in `R_init_cts()` with `R_useDynamicSymbols(dll, TRUE)` to enable standard dynamic symbol resolution. This allows `.Fortran()` calls to correctly resolve registered native routine symbols across all platforms including macOS ARM64.

* **R API Compliance**: Used proper dummy array variables (`DDUM` and `IDUM`) in Fortran code when calling R printing routines (`dblepr`, `intpr`) to comply with Intel Fortran Compiler checking and ensure correct parameter passing for array-expecting functions in `complete.f`.

* **M1 Mac (macOS ARM64) & Clang Compliance**: Added R API headers (`<R.h>`, `<Rinternals.h>`, `<R_ext/Visibility.h>`) to `src/cts_init.c` to ensure proper symbol visibility and eliminate implicit function declaration warnings on strict compilers (like clang 15+).

### Known Issues (LTO Warnings)
* **CSTAK Common Block Type Mismatches** (GCC 15+ with LTO): Link-time optimization in GCC 15+ reports type mismatches in the shared memory stack (`CSTAK`/`DSTAK`) used across multiple compiled units. These are compatibility warnings from legacy Fortran code patterns and do not affect runtime functionality. Consider using `-fno-strict-aliasing` flag if LTO compilation issues occur.

* **Unused Variables and Labels** in Fortran source files (`a9rntc.f`, `a9rnti.f`, etc.): Legacy Fortran code contains unused dummy arguments and labels. These warnings are informational and can be suppressed with compiler flags if needed.