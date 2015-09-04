\name{ba}
\alias{ba}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Batch effect adjustment using a method of choice
}
\description{
Performs batch effect adjustment using one of the following methods: FAbatch, ComBat, SVA, mean-centering, standardization, Ratio-A, Ratio-G or "no batch effect adjustment". Additionally returns information necessary for addon batch effect adjustment with the respective method.
}
\usage{
ba(x, y, batch, method = c("fabatch", "combat", "sva", "meancenter", 
  "standardize", "ratioa", "ratiog", "none"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
matrix. The covariate matrix. observations in rows, variables in columns.
}
  \item{y}{
factor. Binary target variable. Currently has to have levels '1' and '2'. Only used for \code{method = "fabatch"} and \code{method = "sva"}.
}
  \item{batch}{
factor. Batch variable. Currently has to have levels: '1', '2', '3' and so on.
}
  \item{method}{
character. Batch effect adjustment method. 
}
  \item{\dots}{
additional arguments to be passed to \code{fabatch} or \code{svaba}.
}
}
\details{
This function is merely for convenience - a wrapper function for \code{\link{fabatch}}, \code{\link{combatba}}, \code{\link{svaba}}, \code{\link{meancenter}}, \code{\link{standardize}}, \code{\link{ratioa}}, \code{\link{ratiog}} and \code{\link{noba}}.
}
\value{
The output of \code{\link{fabatch}}, \code{\link{combatba}}, \code{\link{svaba}}, \code{\link{meancenter}}, \code{\link{standardize}}, \code{\link{ratioa}}, \code{\link{ratiog}} or \code{\link{noba}} respectively.
}
\references{
Hornung, R., Boulesteix, A.-L., Causeur, D. (2015) Combining location-and-scale batch effect adjustment with data cleaning by latent factor adjustment. Tech. Rep. 184, Department of Statistics, University of Munich.

Johnson, W. E., Rabinovic, A., Li, C. (2007) Adjusting batch effects in microarray expression data using empirical bayes methods. Biostatistics, 8, 118-127.

Leek, J. T., Storey, J. D. (2007) Capturing Heterogeneity in Gene Expression Studies by Surrogate Variable Analysis. PLoS Genetics, 3, 1724--1735.

Luo, J., Schumacher, M., Scherer, A., Sanoudou, D., Megherbi, D., Davison, T., Shi, T., Tong, W., Shi, L., Hong, H., Zhao, C., Elloumi, F., Shi, W., Thomas, R., Lin, S., Tillinghast, G., Liu, G., Zhou, Y., Herman, D., Li, Y., Deng, Y., Fang, H., Bushel, P., Woods, M., Zhang, J. (2010) A comparison of batch effect removal methods for enhancement of prediction performance using maqc-ii microarray gene expression data. The Pharmacogenomics Journal, 10, 278-291.

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


somemethods <- c("fabatch", "combat", "meancenter", "none")

adjusteddata <- list()

for(i in seq(along=somemethods)) {
  cat(paste("Adjusting using method = \"", somemethods[i], "\"", 
    sep=""), "\n")
  adjusteddata[[i]] <- ba(x=Xsub, y=ysub, batch=batchsub, 
    method = somemethods[i])$xadj
}
}
