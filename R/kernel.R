#' Kernel density estimation based on observation.
#'
#' This function generate pdf data for kernel density 
#' estimation. Kernel is simply the N(0,1).
#' @param x Setting of Domain for kernel density estimation
#' @param Xi Observation
#' @keywords kernel
#' @export
#' @examples
#' Xi <- c(3,4.5,5,8,9)
#' x <- seq(min(Xi)-3, max(Xi)+3,by=0.1)
#' sx <- seq(-3,3,by=0.1)
#' 
#' haty <- kde(x,Xi)
#' 
#' plot(x,haty,type="l",ylim=c(0,max(haty)))
#' for(i in 1:length(Xi)){
#'    lines(sx+Xi[i], dnorm(sx)/length(Xi),lty=2) 
#' }
kde <- function(x,Xi) {
  y = rep(0,length(x))
  for(i in 1:length(x)){
    for(j in 1:length(Xi)){
      y[i] = y[i]+dnorm(x[i]-Xi[j])/length(Xi)  
    }
  }
  haty=list(y)
  return(y)
}

