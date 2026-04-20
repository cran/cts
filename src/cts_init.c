#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Fortran calls */
extern void F77_NAME(complete)(double *ss, double *bit);
extern void F77_NAME(cspec)(double *b1, int *arp1, double *scale1,
                            double *frmult, int *nfreq, double *f1,
                            double *s1);
extern void F77_NAME(display)(void);
extern void F77_NAME(forecast)(void);
extern void F77_NAME(kfilsm)(void);
extern void F77_NAME(loop)(double *ss, double *bit, int *errno);
extern void F77_NAME(setcom)(int *pfi1, int *arp1, int *np1, int *vri1,
                             int *ccv1, int *len1, double *scale1,
                             double *vr1, double *sigsq1, double *essp1,
                             double *ecov1, double *b1, double *delb1,
                             double *rootr1, double *rooti1);
extern void F77_NAME(setfor)(double *pre1, double *prv1, double *tim1,
                             double *pre2, double *prv2);
extern void F77_NAME(setkfilsm)(double *fser1, double *fvar1,
                                double *sser1, double *svar1,
                                double *sres1);
extern void F77_NAME(setup)(int *pfi1, int *arp1, int *vri1, int *ccv1,
                            double *scale1, int *ari1, double *vr1,
                            double *phi1, int *lyap1, double *prdg1,
                            int *scc1, int *fct1, int *fty1, int *len1,
                            int *for1, double *tim1, double *ser1,
                            int *nit1, int *opm1, int *rgm1, double *req1,
                            double *concrit1, double *rpert1,
                            double *ivlam1, double *fac1, double *stlam1,
                            double *smlam1, double *gtlam1, int *kst1,
                            int *np1, int *tra1);
extern void F77_NAME(setupdate)(double *phi1);
extern void F77_NAME(update)(void);

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
