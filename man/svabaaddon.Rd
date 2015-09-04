\name{svabaaddon}
\alias{svabaaddon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Addon batch effect adjustment using frozen SVA
}
\description{
Performs addon batch effect adjustment using frozen SVA. Takes the output of performing \code{svaba} on a training data set and new batch data and correspondingly adjusts the test data to better match the adjusted training data according to the SVA model.
}
\usage{
svabaaddon(params, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
object of class \code{svatrain}. Contains parameters necessary for addon batch effect adjustment with frozen SVA.
}
  \item{x}{
matrix. The covariate matrix of the new data. Observations in rows, variables in columns.
}
}

\value{
The adjusted covariate matrix of the test data.
}
\references{
Leek, J. T., Storey, J. D. (2007) Capturing Heterogeneity in Gene Expression Studies by Surrogate Variable Analysis. PLoS Genetics, 3, 1724--1735.

Parker, H. S., Bravo, H. C., Leek, J. T. (2014) Removing batch effects for prediction problems with frozen surrogate variable analysis. PeerJ, 2, e561.
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



trainind <- which(batchsub \%in\% c(1,2))

Xsubtrain <- Xsub[trainind,]
ysubtrain <- ysub[trainind]
batchsubtrain <- factor(as.numeric(batchsub[trainind]), levels=c(1,2))


testind <- which(batchsub \%in\% c(3,4))

Xsubtest <- Xsub[testind,]
ysubtest <- ysub[testind]

batchsubtest <- as.numeric(batchsub[testind])
batchsubtest[batchsubtest==3] <- 1
batchsubtest[batchsubtest==4] <- 2
batchsubtest <- factor(batchsubtest, levels=c(1,2))



params <- svaba(x=Xsubtrain, y=ysubtrain, batch=batchsubtrain)

Xsubtestaddon <- svabaaddon(params, x=Xsubtest)
}
