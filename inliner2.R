dll <- "~/Work/Haskell/inliner2/foo2.so"
dyn.load(dll)
.C("HsStart")

.C("rangeR", a=1L, b=4L, result=list(NULL))$result[[1]]
library(microbenchmark)
a <- 1L; b <- 100L
microbenchmark(
  HC = .C("rangeR", a=a, b=b, result=list(NULL))$result[[1]],
  R = a:b
)

f <- function(x) x+1
.C("test_myevalR", x=3, result=0)$result

f <- function(x) cos(4*acos(x))
library(microbenchmark)
n <- 15L
microbenchmark(
  R2 = .C("chebyshevFitR2", n=n, result=list(0))$result[[1L]],
  R3 = .C("chebyshevFitR3", n=n, result=list(0))$result[[1L]],
  R4 = .C("chebyshevFitR4", n=n, result=list(0))$result[[1L]],
  R5 = .C("chebyshevFitR5", f=list(f), n=n, result=list(0))$result[[1L]]
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
# oui mais pracma plante:
x <- seq(-0.9, 0.9, length.out = 10)
n <- 5L
.C("chebApproxR", n=n, f=list(f), x = list(x), result=list(0))$result[[1L]]
chebApprox(x, f, -1, 1, n-1)
n <- 100L
.C("chebApproxR", n=n, f=list(f), x = list(x), result=list(0))$result[[1L]]
chebApprox(x, f, -1, 1, n-1)

dyn.unload(dll)


