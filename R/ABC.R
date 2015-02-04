#' @param N The numebr of particles to be used in the sampler
#' @param epsilon The percentage of the quantile of the distance
#' @param \code{rho} The distance measure 
#' @param \code{eta} The summary statistics
#' @param prior The prior sampler
#' @param f The likelihood function
#' @return \code{N*epsilon} samples from the approximated target distribution
#' @author Xiaoyu Lu <xiaoyu.lu@stats.ox.ac.uk>
#' @export
#' @examples 


ABC = function(N,epsilon,rho,eta,prior,f) {
  z = c(); x = c() ;dist = c()
  
  ## posterior of theta
  theta = matrix(,N,length(prior()))
    
    for (j in 1:N) {    
      #propose theta
      x = prior()      
      #generate z from the likelihood
      z = f(x)
      
      dist[j] = rho(eta(y),eta(z))
      theta[j,] = x     
  }
  accept = which(dist<quantile(dist,epsilon)[[1]])
  return(theta[accept,])
}  
