library(moments)
library(Metrics)

errMeasure1 <- function (vPred, vTarget)
{
  a = cbind(vPred,vTarget);
  ret = apply(a,MARGIN=1,function(x) {v=unlist(strsplit(unlist(x[1]),split=" ")); ix = which(v==trimws(x[2]));r=0; if(length(ix) != 0 && ix<=12) r=1/(ix); return(r)});
  #return(list(mean(ret),ret))
  return(mean(ret));
}







