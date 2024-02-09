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


# Function to clean latitude/longitude coordinates from EFC dataset 
get.decimal.coord <- function(coord){
  # Some entries have elevation after a comma
  coord = strsplit(coord, ",")[[1]][1]
  coord = strsplit(coord, ';')[[1]]
  dec.coord <- c(NA, NA)
  if(length(coord)==2){
    for(i in 1:2){
      s1 = strsplit(coord[i], "°")[[1]]
      s2 = strsplit(s1[2], "'")[[1]]
      s3 = strsplit(s2[2], "\"")[[1]]
      c1 = as.numeric(s1[1])
      c2 = as.numeric(s2[1])
      c3 = as.numeric(s3[1])
      dec.coord[i] <- c1 + c2/60 + c3/3600
    }
  }
  return(dec.coord)
}


# Function to summarize parameters in terms of both HPDIs and PIs
parameter_summary <- function(df, prob) {
  
  summary <- df %>%
    tidyr::pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
    group_by(parameter) %>%
    summarize(
      mean = mean(value),
      lower_HPDI = HPDI(value, prob = prob)[1],
      upper_HPDI = HPDI(value, prob = prob)[2],
      lower_PI = PI(value, prob = prob)[1],
      upper_PI = PI(value, prob = prob)[2],
      width_HPDI = upper_HPDI - lower_HPDI,
      width_PI = upper_PI - lower_PI
    ) %>%
    ungroup()
  
  return(summary)
}
