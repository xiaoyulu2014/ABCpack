\name{ABC}
\alias{ABC}
\title{Approximate Bayesian Computation}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABC(N, epsilon, rho, eta, prior, f)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
The numebr of particles to be used in the sampler
}
  \item{epsilon}{
The percentage of the quantile of the distance
}
  \item{rho}{
The distance measure 
}
  \item{eta}{
The summary statistics
}
  \item{prior}{
The prior sampler
}
  \item{f}{
The likelihood function
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{N*epsilon} samples from the approximated target distribution
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Xiaoyu Lu}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

prior = function() {
  a = runif(2)
  x = c(-2*sqrt(a[1]) + 4*a[2]*sqrt(a[1]),2*sqrt(a[1])-1)
  return(x)
}

f = function(x) {
  u = rnorm(n+q, 0, 1)
  for (k in 1:n) {
    z[k] = u[k+q] + t(x) %*% u[(k-1+q):k]
  }
  return(z)
}
ABC(100000,0.01,function(y,z) sqrt(sum((y-z)^2)) ,function(x) x,prior,f)
