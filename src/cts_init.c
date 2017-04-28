#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 *    Check these declarations against the C/Fortran source code.
 *    */

/* .Fortran calls */
extern void F77_NAME(complete)(void *, void *);
extern void F77_NAME(cspec)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(display)();
extern void F77_NAME(forecast)();
extern void F77_NAME(kfilsm)();
extern void F77_NAME(loop)(void *, void *, void *);
extern void F77_NAME(setcom)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(setfor)(void *, void *, void *, void *, void *);
extern void F77_NAME(setkfilsm)(void *, void *, void *, void *, void *);
extern void F77_NAME(setup)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(setupdate)(void *);
extern void F77_NAME(update)();

static const R_FortranMethodDef FortranEntries[] = {
	    {"complete",  (DL_FUNC) &F77_NAME(complete),   2},    
	    {"cspec",     (DL_FUNC) &F77_NAME(cspec),      7},
            {"display",   (DL_FUNC) &F77_NAME(display),    0},
	    {"forecast",  (DL_FUNC) &F77_NAME(forecast),   0},
	    {"kfilsm",    (DL_FUNC) &F77_NAME(kfilsm),     0},
	    {"loop",      (DL_FUNC) &F77_NAME(loop),       3},
	    {"setcom",    (DL_FUNC) &F77_NAME(setcom),    15},
	    {"setfor",    (DL_FUNC) &F77_NAME(setfor),     5},
	    {"setkfilsm", (DL_FUNC) &F77_NAME(setkfilsm),  5},
	    {"setup",     (DL_FUNC) &F77_NAME(setup),     31},
	    {"setupdate", (DL_FUNC) &F77_NAME(setupdate),  1},
	    {"update",    (DL_FUNC) &F77_NAME(update),     0},
	    {NULL, NULL, 0}
};

void R_init_cts(DllInfo *dll)
{   
	    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
	    R_useDynamicSymbols(dll, FALSE);
}

