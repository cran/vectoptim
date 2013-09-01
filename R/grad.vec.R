grad.vec <-
function(func, x, method="simple", method.args=list(), vec=FALSE, core=2){
	if(vec=="foreach"){
		registerDoParallel(core)

	} else if(vec!=FALSE){
		stop(paste("vectorize type '",vec,"' not supported.",sep=""))
	}
	
	f <- func(x)
	n <- length(x)	 #number of variables in argument
	case1or3 <- n == length(f)
	if((1 != length(f)) & !case1or3)
		stop("grad assumes a scalar valued function.")
	if(method=="simple"){
		#  very simple numerical approximation
		args <- list(eps=1e-4) # default
		args[names(method.args)] <- method.args
		eps <- args$eps
		if(case1or3) return((func(x+eps)-f)/eps) 
		# now case 2
		df <- rep(NA,n)
		if(vec==FALSE){
			for (i in 1:n) {
				dx <- x
				dx[i] <- dx[i] +eps 
				df[i] <- (func(dx)-f)/eps
			}
		
		} else if (vec=="foreach") {
			temp <-matrix(rep(x,n), n, n)
			temp <-temp+diag(n)*eps
			demo<-foreach(i=1:n) %do% { (func(temp[,i])-f)/eps}
			df<-matrix(unlist(demo),1,n)
		
		} else {
			stop(paste("vectorize type '",vec,"' not supported.",sep=""))
		}

		return(df)
    } else 	if(method=="complex"){ # Complex step gradient
		eps <- .Machine$double.eps
		v <- try(func(x + eps * 1i))
		if(inherits(v, "try-error")) 
			stop("function does not accept complex argument as required by method 'complex'.")
		if(!is.complex(v)) 
			stop("function does not return a complex value as required by method 'complex'.")
   
		if(case1or3) return(Im(v)/eps) 
		# now case 2
		h0 <- rep(0, n)
		g  <- rep(NA, n)
		if(vec==FALSE){
			for (i in 1:n) {
				h0[i] <- eps * 1i
				g[i] <- Im(func(x+h0))/eps 
				h0[i]  <- 0
			}
		} else if (vec=="foreach") {
			temp <-matrix(0, n, n)
			temp <-temp+diag(n)*eps*1i
			demo<-foreach(i=1:n) %dopar% { Im(func(temp[,i]))/eps}
			df<-matrix(unlist(demo),length(f),n)
    
		} else {
			stop(paste("vectorize type '",vec,"' not supported.",sep=""))
    }
	return(g)
    } else stop("indicated method ", method, "not supported.")

		
	
}
