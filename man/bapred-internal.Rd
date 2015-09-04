\name{bapred-internal} 
\title{Internal bapred functions}
\alias{Dt}
\alias{VarInflation}
\alias{aprior}
\alias{avedistTwo}
\alias{bivprob}
\alias{bprior}
\alias{build.design}
\alias{design.mat}
\alias{diffexprmAfterBR}
\alias{emfahighdim}
\alias{fuzzywilcox}
\alias{it.sol}
\alias{kldistTwo}
\alias{list.batch}
\alias{mypvcaBatchAssess}
\alias{nbfactors}
\alias{plotcomp}
\alias{postmean}
\alias{postvar}
\alias{print.combat}
\alias{print.fabatch}
\alias{print.meancenter}
\alias{print.noba}
\alias{print.ratioa}
\alias{print.ratiog}
\alias{print.standardize}
\alias{print.svatrain}
\alias{sepscoreTwo}
\alias{skewdivTwo}
\description{
  Internal bapred functions.
}
\usage{
Dt(rho) 
VarInflation(dta, Blist, maxnbfactors, dig) 
aprior(gamma.hat) 
avedistTwo(xb1, xb2) 
bivprob(rho, lower, upper = -lower, mean = 0) 
bprior(gamma.hat) 
build.design(vec, des = NULL, start = 2) 
design.mat(mod, numCov) 
diffexprmAfterBR(x, xbrlist, y, batch, batchessuited) 
emfahighdim(eps, nbf, minerr = 1e-06, maxiter = 100) 
fuzzywilcox(x, y) 
it.sol(sdat, g.hat, d.hat, g.bar, t2, a, b, conv = 1e-04) 
kldistTwo(xb1, xb2) 
list.batch(mod) 
mypvcaBatchAssess(X, factordata, threshold) 
nbfactors(dta, maxnbfactors = 12, diagnostic.plot = FALSE, minerr = 0.001, 
  maxiter = 100) 
plotcomp(xp, groups, y, alpha, col = rainbow(length(groupsun)), 
  xlim, ylim, ...) 
postmean(g.hat, g.bar, n, d.star, t2) 
postvar(sum2, n, a, b) 
\method{print}{combat}(x, ...) 
\method{print}{fabatch}(x, ...) 
\method{print}{meancenter}(x, ...) 
\method{print}{noba}(x, ...) 
\method{print}{ratioa}(x, ...) 
\method{print}{ratiog}(x, ...) 
\method{print}{standardize}(x, ...) 
\method{print}{svatrain}(x, ...) 
sepscoreTwo(xb1, xb2, k = 10) 
skewdivTwo(xb1, xb2)
}
\details{
  These internal \pkg{bapred} functions are not usually called
  directly by the user.
}
\keyword{internal}
