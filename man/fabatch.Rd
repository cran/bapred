\name{fabatch}
\alias{fabatch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Batch effect adjustment using FAbatch
}
\description{
Performs batch effect adjustment using the FAbatch-method described in Hornung et al. (2015) and additionally returns information necessary for addon batch effect adjustment with FAbatch.
}
\usage{
fabatch(x, y, batch, nbf = NULL, minerr = 1e-06, 
  probcrossbatch = TRUE, maxiter = 100, maxnbf = 12)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
matrix. The covariate matrix. Observations in rows, variables in columns.
}
  \item{y}{
factor. Binary target variable. Currently has to have levels '1' and '2'.
}
  \item{batch}{
factor. Batch variable. Currently has to have levels: '1', '2', '3' and so on.
}
  \item{nbf}{
integer. Number of factors to estimate in all batches. If not given the number of factors is estimated automatically for each batch. Recommended to leave unspecified.
}
  \item{minerr}{
numeric. Maximal mean quadratic deviations between the estimated residual variances from two consecutive iterations. The iteration stops when this value is undercut.
}
  \item{probcrossbatch}{
logical. Default is \code{TRUE}. If \code{TRUE} the preliminary probabilities are estimated through leave-one-batch-out cross-validation. If set to \code{FALSE} ordinary cross-validation is used for estimating the preliminary probabilities. This might result in an artificially increased class signal in comparison to that in the data in independent batches. Is automatically set to \code{FALSE}, when only one batch is present in the training data.
}
  \item{maxiter}{
integer. Maximal number of iterations in the estimation of the latent factors by Maximum Likelihood.
}
  \item{maxnbf}{
integer. Maximal number of factors if \code{nbf} is not given. Default is the largest integer smaller than half the number of observations in the corresponding batch.
}
}

\value{
\code{fabatch} returns an object of class \code{fabatch}.
An object of class "\code{fabatch}" is a list containing the following components: 
\item{xadj}{ matrix of adjusted (training) data }
\item{m1}{ means of the standardized variables in class '1' }
\item{m2}{ means of the standardized variables in class '2' }
\item{b0}{ intercept out of the L2-penalized logistic regression performed for estimation of the class probabilities }
\item{b}{ variable coefficients out of the L2-penalized logistic regression performed for estimation of the class probabilities }
\item{pooledsds}{ vector containing the pooled standard deviations of the variables }
\item{meanoverall}{ vector containing the variable means }
\item{minerr}{ maximal mean quadratic deviations between the estimated residual variances from two consecutive iterations }
\item{nbfinput}{ user-specified number of latent factors \code{nbf} in all batches. \code{NULL} if \code{nbf} was not specified. }
\item{badvariables}{ indices of those variables which are constant in at least one batch }
\item{nbatches}{ number of batches }
\item{batch}{ batch variable }
\item{nbfvec}{ vector containing the numbers of factors in the individual batches }
}
\references{
Hornung, R., Boulesteix, A.-L., Causeur, D. (2015) Combining location-and-scale batch effect adjustment with data cleaning by latent factor adjustment. Tech. Rep. 184, Department of Statistics, University of Munich.
}
\author{
Roman Hornung
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(autism)

# Random subset of 150 variables:
set.seed(1234)
Xsub <- X[,sample(1:ncol(X), size=150)]

# In cases of batches with more than 20 observations
# select 20 observations at random:
subinds <- unlist(sapply(1:length(levels(batch)), function(x) {
  indbatch <- which(batch==x)
  if(length(indbatch) > 20)
    indbatch <- sort(sample(indbatch, size=20))
  indbatch
}))
Xsub <- Xsub[subinds,]
batchsub <- batch[subinds]
ysub <- y[subinds]



fabatch(x=Xsub, y=ysub, batch=batchsub)
}
