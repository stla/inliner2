dll <- "~/Work/Haskell/inliner2/foo2.so"
dll <- "C:/HaskellProjects/inliner2/foo2.dll"
dll <- "C:/HaskellProjects/inliner2/fooTest.dll"
dyn.load(dll)
.C("HsStart")

.C("test00", result=list(NULL))$result[[1]]

.C("test01", file="example.xlsx", sheet="Sheet1", index=1L, result=list(NULL))$result[[1]]


.Call("vectorAppendR", list(1,2), "a")
.Call("vectorAppendR", NULL, "a")

.C("whichR", vectorR = list(c(1,2,3,4,5)), a=3, result=list(0L))$result[[1L]]
library(microbenchmark)
x <- as.double(rpois(100, 10))
microbenchmark(
  H = .C("whichR", vectorR=list(x), a=10, result=list(0L))$result[[1L]],
  R = which(x==10)
)

.C("sliceR", vectorR = list(c(1,2,3,4,5)), i=1L, n=2L, result=list(0))$result[[1L]]
library(microbenchmark)
x <- rnorm(1e6)
i <- 7000L; n <- 900000L
microbenchmark(
  H = .C("sliceR", vectorR=list(x), i=i, n=n, result=list(0))$result[[1L]],
  R = x[(i+1L):(i+1L+n)]
) # Haskell meilleur !

.C("rangeR", a=1L, b=4L, result=list(NULL))$result[[1]]
library(microbenchmark)
a <- 1L; b <- 1000L
microbenchmark(
  HC = .C("rangeR", a=a, b=b, result=list(NULL))$result[[1]],
  R = a:b
)

f <- function(x) x+1
.C("test_myevalR", x=3, result=0)$result

.C("test_myeval2R", f=list(function(x) x+1), x=3, result=0)$result


f <- function(x) cos(4*acos(x))
library(microbenchmark)
library(pracma)
n <- 15L
microbenchmark(
  R2 = .C("chebyshevFitR2", n=n, result=list(0))$result[[1L]],
  R3 = .C("chebyshevFitR3", n=n, result=list(0))$result[[1L]],
  R4 = .C("chebyshevFitR4", n=n, result=list(0))$result[[1L]],
  R5 = .C("chebyshevFitR5", f=list(f), n=n, result=list(0))$result[[1L]],
  P = chebCoeff(f, -1, 1, n-1)
)

f <- function(x) exp(x)
f <- function(x) cos(4*acos(x))
x <- c(-0.5, -0.2, 0, 0.2, 0.5)
n <- 5L
.C("chebApproxR", n=n, f=list(f), x = list(x), result=list(0))$result[[1L]]
library(pracma)
chebApprox(x, f, -1, 1, n-1)
library(microbenchmark)
n <- 5L
x <- seq(-0.9, 0.9, length.out = 10)
microbenchmark(
  H = .C("chebApproxR", n=n, f=list(f), x = list(x), result=list(0))$result[[1L]],
  R = chebApprox(x, f, -1, 1, n-1)
)
# plus lent oui mais pracma plante:
x <- seq(-0.9, 0.9, length.out = 10)
n <- 5L
.C("chebApproxR", n=n, f=list(f), x = list(x), result=list(0))$result[[1L]]
chebApprox(x, f, -1, 1, n-1)
n <- 100L
.C("chebApproxR", n=n, f=list(f), x = list(x), result=list(0))$result[[1L]]
chebApprox(x, f, -1, 1, n-1)

dyn.unload(dll)


