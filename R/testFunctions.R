schwef <-
function(xx, delay=0)
{
  Sys.sleep(delay)
  d <- length(xx)
  sum <- sum(xx*sin(sqrt(abs(xx))))
  y <- 418.9829*d - sum
  return(y)
}

rosenbrock <-
function (xx, delay=0) 
{
  Sys.sleep(delay)
  n <- length(xx)
  xi <- xx[seq_len(n - 1L)]
  sum(100 * (xx[2L:n] - xi * xi)^2 + (1 - xi)^2)
}


rastr <-
function(xx, delay=0)
{
  Sys.sleep(delay)
  d <- length(xx)
  sum <- sum(xx^2 - 10*cos(2*pi*xx))
  y <- 10*d + sum
  return(y)
}

powell <-
function(xx, delay=0)
{
  Sys.sleep(delay)
  d <- length(xx)
	
  xxa <- xx[seq(1, d-3, 4)]
  xxb <- xx[seq(2, d-2, 4)]
  xxc <- xx[seq(3, d-1, 4)]
  xxd <- xx[seq(4, d, 4)]

  sumterm1 <- (xxa + 10*xxb)^2
  sumterm2 <- 5 * (xxc - xxd)^2
  sumterm3 <- (xxb - 2*xxc)^4
  sumterm4 <- 10 * (xxa - xxd)^4
  sum <- sum(sumterm1 + sumterm2 + sumterm3 + sumterm4)
	
  y <- sum
  return(y)
}

levy <-
function(xx, delay=0)
{
  Sys.sleep(delay)
  d <- length(xx)
  w <- 1 + (xx - 1)/4
	
  term1 <- (sin(pi*w[1]))^2 
  term3 <- (w[d]-1)^2 * (1+1*(sin(2*pi*w[d]))^2)
	
  wi <- w[1:(d-1)]
  sum <- sum((wi-1)^2 * (1+10*(sin(pi*wi+1))^2))
	
  y <- term1 + sum + term3
  return(y)
}

ackley <-
function(xx, delay=0, a=20, b=0.2, c=2*pi)
{
  Sys.sleep(delay)
  n <- length(xx)
  
  sum1 <- sum(xx^2)
  sum2 <- sum(cos(c*xx))

  term1 <- -a * exp(-b*sqrt(sum1/n))
  term2 <- -exp(sum2/n)

  y <- term1 + term2 + a + exp(1)
  return(y)
}
