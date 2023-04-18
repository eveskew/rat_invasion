precis_plot <- function( x , y , pars , col.ci="black" , xlab="Value" , add=FALSE , xlim=NULL , labels=rownames(x)[1:n] , ... ) {
  if ( !missing(pars) ) {
    x <- x[pars,]
  }
  n <- nrow(x)
  mu <- x[n:1,1]
  left <- x[[3]][n:1]
  right <- x[[4]][n:1]
  set_nice_margins()
  labels <- labels[n:1]
  if ( is.null(xlim) ) xlim <- c(min(left),max(right))
  if ( add==FALSE )
    dotchart( mu , labels=labels , xlab=xlab , xlim=xlim , ... )
  else
    points( mu[n:1] , n:1 , ... )
  for ( i in 1:length(mu) ) lines( c(left[i],right[i]) , c(i,i) , lwd=2 , col=col.ci )
  if ( add==FALSE ) abline( v=0 , lty=1 , col=col.alpha("black",0.15) )
}
setMethod( "plot" , "precis" , function(x,y,...) precis_plot(x,y,...) )