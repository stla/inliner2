#include <R.h>
#include <Rinternals.h>

SEXP rangeSEXP(int a, int b){
    int i;
    SEXP Rval;
    PROTECT(Rval = allocVector(INTSXP, b - a + 1));
    for (i = a; i <= b; i++)
        INTEGER(Rval)[i - a] = i;
    UNPROTECT(1);
    return Rval;
}

double myeval(double x) {
    // convert x to SEXP
    SEXP xR;
    PROTECT(xR = allocVector(REALSXP, 1));
    REAL(xR)[0] = x;
    UNPROTECT(1);
    SEXP call;
    call = Rf_lang2(install("f"), xR); 
    // evaluate f(x)
    return(REAL(eval(call, R_GlobalEnv))[0]);
}

double myeval2(SEXP f, double x) {
    // convert x to SEXP
    SEXP xR;
    PROTECT(xR = allocVector(REALSXP, 1));
    REAL(xR)[0] = x;
    UNPROTECT(1);
    // put f in an environment
    SEXP rho = allocSExp(ENVSXP);
    SEXP f_symbol = install("f");
    defineVar(f_symbol, f, rho);
    // evaluate f(x)
    SEXP call = Rf_lang2(f_symbol, xR); 
    return(REAL(eval(call, rho))[0]);
}

SEXP realToSEXP (int n, double *arr){
    SEXP Rval;
    PROTECT(Rval = allocVector(REALSXP, n));
    for (int i = 0; i < n; i++)
        REAL(Rval)[i] = arr[i];
    UNPROTECT(1);
    return Rval;
}

double* SEXPtoReal (SEXP vectorR){
    int n = length(vectorR);
    double* vector = REAL(vectorR);
    double* list = calloc(n, sizeof(double));
    for(int i=0; i<n; i++){
        list[i] = vector[i];
    }
    return list;
}