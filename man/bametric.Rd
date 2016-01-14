\name{bametric}
\alias{bametric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Diverse metrics for quality of (adjusted) batch data
}
\description{
Diverse metrics measuring the severeness of batch effects in (batch effect adjusted) data or the performance of certain analyses performed using the latter. This is a wrapper function for the following functions, where each of them calculates a certain metric: \code{\link{sepscore}}, \code{\link{avedist}}, \code{\link{kldist}}, \code{\link{skewdiv}}, \code{\link{pvcam}}, \code{\link{diffexprm}} and \code{\link{corba}}. For details see the documentation of the latter.
}
\usage{
bametric(xba, batch, y, x, metric = c("sep", "avedist", "kldist", 
  "skew", "pvca", "diffexpr", "cor"), method, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xba}{
matrix. The covariate matrix, raw or after batch effect adjustment. observations in rows, variables in columns.
}
  \item{batch}{
factor. Batch variable. Currently has to have levels: '1', '2', '3' and so on.
}
  \item{y}{
factor. Binary target variable. Currently has to have levels '1' and '2'. Only used for \code{metric = "pvca"} and \code{metric = "diffexpr"}.
}
  \item{x}{
matrix. The covariate matrix before batch effect adjustment. observations in rows, variables in columns.
}
  \item{metric}{
character. The metric to use. 
}
  \item{method}{
character. The batch effect adjustment method to use for \code{metric = "diffexpr"}.
}
  \item{\dots}{
additional arguments to be passed to \code{sepscore} or \code{pvcam}.
}
}

\value{
Value of the respective metric
}
\references{
Boltz, S., Debreuve, E., Barlaud, M. (2009) High-dimensional statistical measure for region-of-interest tracking. Transactions in Image Processing, 18(6), 1266-1283.

Geyer, C. J., Meeden, G., D. (2005) Fuzzy and randomized confidence intervals and p-values (with discussion). Statistical Science, 20(4), 358-387.

Hornung, R., Boulesteix, A.-L., Causeur, D. (2016) Combining location-and-scale batch effect adjustment with data cleaning by latent factor adjustment. BMC Bioinformatics 17:27.

Lazar, C., Meganck, S., Taminau, J., Steenhoff, D., Coletta, A., Molter,C., Weiss-Solís,
D. Y., Duque, R., Bersini, H., Nowé, A. (2012) Batch effect removal methods for
microarray gene expression data integration: a survey. Briefings in Bioinformatics, 14(4), 469-490.

Lee, J. A., Dobbin, K. K., Ahn, J. (2014) Covariance adjustment for batch effect in gene expression data. Statistics in Medicine, 33, 2681-2695.

Li, J., Bushel, P., Chu, T.-M., Wolfinger, R.D. (2009) Principal variance components analysis: Estimating batch effects in microarray gene expression data. In: Scherer, A. (ed) Batch Effects and Noise in Microarray Experiments: Sources and Solutions, John Wiley & Sons, Chichester, UK.

Shabalin, A. A., Tjelmeland, H., Fan, C., Perou, C. M., Nobel, A. B. (2008) Merging two gene-expression studies via cross-platform normalization. Bioinformatics, 24(9), 1154-1160.
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




Xsubadj <- ba(x=Xsub, y=ysub, batch=batchsub, method = "combat")$xadj


bametric(xba=Xsub, batch=batchsub, metric = "sep")
bametric(xba=Xsubadj, batch=batchsub, metric = "sep")

bametric(xba=Xsub, batch=batchsub, metric = "avedist")
bametric(xba=Xsubadj, batch=batchsub, metric = "avedist")

bametric(xba=Xsub, batch=batchsub, metric = "kldist")
bametric(xba=Xsubadj, batch=batchsub, metric = "kldist")

bametric(xba=Xsub, batch=batchsub, metric = "skew")
bametric(xba=Xsubadj, batch=batchsub, metric = "skew")

bametric(xba=Xsub, batch=batchsub, y=ysub, metric = "pvca")
bametric(xba=Xsubadj, batch=batchsub, y=ysub, metric = "pvca")

bametric(x=Xsub, batch=batchsub, y=ysub, metric = "diffexpr", 
  method = "combat")

bametric(xba=Xsubadj, x=Xsub, metric = "cor")
}
