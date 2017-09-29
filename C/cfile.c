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
    //int n = length(vectorR);
    double* vector = REAL(vectorR);
    //double* list = calloc(n, sizeof(double));
    //for(int i=0; i<n; i++){
    //    list[i] = vector[i];
    //}
    //return list;
    return vector;
}

SEXP intToSEXP (int n, int *arr){
    SEXP Rval;
    PROTECT(Rval = allocVector(INTSXP, n));
    for (int i = 0; i < n; i++)
        INTEGER(Rval)[i] = arr[i];
    UNPROTECT(1);
    return Rval;
}

SEXP vectorAppend(SEXP list, SEXP x) {
  SEXP new; int i;
  int n = length(list);
  PROTECT(new=allocVector(VECSXP, n+1));
  for(i=0;i<n;i++)
    SET_VECTOR_ELT(new, i, Rf_duplicate(VECTOR_ELT(list, i)));
  SET_VECTOR_ELT(new, n, x);
  UNPROTECT(1);
  return new;
}

SEXP allocProtectedVector(int n){
    SEXP new;
    PROTECT(new = allocVector(VECSXP, n));
    return new;
}

void writeInVector(SEXP vector, SEXP element, int index){
    SET_VECTOR_ELT(vector, index, element);
    return ;
}

SEXP singletonVector(SEXP x){
  return vectorAppend(R_NilValue, x);
}

SEXP null0(){
  return R_NilValue;
}

SEXP mkVector(SEXP *list, size_t n){
    SEXP out; int i;
    PROTECT(out=allocVector(VECSXP, n));
    for(i=0;i<n;i++)
        SET_VECTOR_ELT(out, i, list[i]);
    UNPROTECT(1);
    return out;
}