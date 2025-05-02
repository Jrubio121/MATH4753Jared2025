#' Newton-Raphson Maximum Likelihood Estimation
#'
#' Uses the Newton-Raphson method to find the value of a parameter that maximizes the log-likelihood function.
#'
#' @param x0 Starting value for the parameter.
#' @param delta Convergence threshold and step size for numerical derivative.
#' @param llik Log-likelihood function to be maximized.
#' @param xrange Range of x-values to use for plotting.
#' @param parameter Name of the parameter (used for labeling the x-axis).
#'
#' @return A list with vectors `x` (parameter estimates at each step) and `y` (derivatives at those steps).
#' @export
#'
#' @examples
#' \dontrun{myNRML(x0 = 1, delta = 0.0001,
#'        llik = function(lambda) log(dpois(4, lambda)),
#'        xrange = c(0, 10), parameter = "lambda")}
myNRML=function(x0,delta=0.001,llik,xrange,parameter="param"){
  f=function(x) (llik(x+delta)-llik(x))/delta
  fdash=function(x) (f(x+delta)-f(x))/delta
  d=1000
  i=0
  x=c()
  y=c()
  x[1]=x0
  y[1]=f(x[1])
  while(d > delta & i<100){
    i=i+1
    x[i+1]=x[i]-f(x[i])/fdash(x[i])
    y[i+1]=f(x[i+1])
    d=abs(y[i+1])
  }
  layout(matrix(1:2,nr=1,nc=2,byrow=TRUE),width=c(1,2))
  curve(llik(x), xlim=xrange,xlab=parameter,ylab="log Lik",main="Log Lik")
  curve(f(x),xlim=xrange,xaxt="n", xlab=parameter,ylab="derivative",main=  "Newton-Raphson Algorithm \n on the derivative")
  points(x,y,col="Red",pch=19,cex=1.5)
  axis(1,x,round(x,2),las=2)
  abline(h=0,col="Red")

  segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=2)
  segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Green")

  list(x=x,y=y)
}
