\name{fabatchaddon}
\alias{fabatchaddon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Addon batch effect adjustment using FAbatch
}
\description{
Performs addon batch effect adjustment using FAbatch. Takes the output of performing \code{\link{fabatch}} on a training data set and new batch data and correspondingly adjusts the test data to better match the adjusted training data according to the FAbatch model.
}
\usage{
fabatchaddon(params, x, batch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
object of class \code{fabatch}. Contains parameters necessary for addon batch effect adjustment.
}
  \item{x}{
matrix. The covariate matrix of the new data. Observations in rows, variables in columns.
}
  \item{batch}{
factor. Batch variable of the new data. Currently has to have levels: '1', '2', '3' and so on.
}
}

\value{
The adjusted covariate matrix of the test data.
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



params <- fabatch(x=Xsubtrain, y=ysubtrain, batch=batchsubtrain)

Xsubtestaddon <- fabatchaddon(params, x=Xsubtest, 
  batch=batchsubtest)
}