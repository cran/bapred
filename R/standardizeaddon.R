standardizeaddon <-
function(params, x, batch) {

  if(any(is.na(x)))
	stop("Data contains missing values.")
  if(!is.factor(batch))
    stop("'batch' has to be of class 'factor'.")  
  if(!is.matrix(x))
    stop("'x' has to be of class 'matrix'.")

  if(!inherits(params, "standardize"))
     stop("Input parameter 'params' has to be of class 'standardize'.")

  if(ncol(params$xadj) != ncol(x))
    stop("Number of variables in test data matrix different to that of training data matrix.")	 
	 
   batches = levels(batch)
   nbatches = length(batches)
   
   if(nbatches > 1) {
     # Adjust for additive batch effects (Reference batch?!):
     adjustmentmod = lm(x~batch)
     design = model.matrix(~batch)
     adjustmentcoef = coef(adjustmentmod)
     xcenter = x-design%*%adjustmentcoef
   }
   else {
	 adjustmentcoef <- colMeans(x)
     xcenter = scale(x, center=adjustmentcoef, scale=FALSE)	 
   }
   
   # Calculate batch-specific standard deviations:
   sdb = as.list(rep(0,nbatches))
   for (i in 1:nbatches) {
      sdb[[i]] = apply(xcenter[batch==batches[i],],2,sd)
   }
      
   xadj = xcenter
   for (i in 1:nbatches) {
     sdabovezero <- which(sdb[[i]]!=0)
     xadj[batch==batches[i],sdabovezero] = scale(xcenter[batch==batches[i],sdabovezero],center=rep(0,ncol(xcenter[,sdabovezero])),scale=sdb[[i]][sdabovezero])
   }

   return(xadj) 

}
