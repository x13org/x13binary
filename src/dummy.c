// dummy.c
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void dummy(void) {
    // This is a dummy function to satisfy R package checks for source files
    // in the src directory.
}

// Registration
static const R_CallMethodDef callMethods[]  = {
  {"dummy", (DL_FUNC) &dummy, 0},
  {NULL, NULL, 0}
};

void R_init_x13binary(DllInfo *dll) {
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
