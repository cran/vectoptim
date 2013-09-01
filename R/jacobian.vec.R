jacobian.vec <-
function(func, x, method="simple", method.args=list(), vec=FALSE, core=2){

	if(vec=="foreach"){
		registerDoParallel(core)

	} else if(vec!=FALSE){
		stop(paste("vectorize type '",vec,"' not supported.",sep=""))
	}

	f <- func(x)
	n <- length(x)	 #number of variables.
	if(method=="simple"){
		args <- list(eps=1e-4) # default
		args[names(method.args)] <- method.args
		eps <- args$eps
		df <-matrix(NA, length(f), n)
		if(vec==FALSE){
			for (i in 1:n) {
				dx <- x
				dx[i] <- dx[i] +eps 
				df[,i] <- (func(dx)-f)/eps
			}
		
		} else if (vec=="foreach") {
			temp <-matrix(rep(x,n), n, n)
			temp <-temp+diag(n)*eps
			demo<-foreach(i=1:n) %dopar% { (func(temp[,i])-f)/eps}
			df<-matrix(unlist(demo),length(f),n)
		
		} 
		return(df)
	} else if(method=="complex"){ 
	eps <- .Machine$double.eps
	h0  <-  rep(0, n)
    h0[1] <- eps * 1i
    v <- try(func(x+h0))
    if(inherits(v, "try-error")) 
		stop("function does not accept complex argument as required by method 'complex'.")
    if(!is.complex(v)) 
		stop("function does not return a complex value as required by method 'complex'.")
  
    h0[1]  <- 0
    jac <- matrix(NA, length(v), n)
    jac[, 1] <- Im(v)/eps
    if (n == 1) return(jac)
    else {
		if(vec==FALSE){
			for (i in 2:n) {
			  h0[i] <- eps * 1i
			  jac[, i] <- Im(func(x+h0))/eps 
			  h0[i]  <- 0
			}
		
		} else if (vec=="foreach") {
			temp <-matrix(rep(h0,n), n, n)
			temp <-temp+diag(n)*eps*1i
			demo<-foreach(i=1:n) %dopar% { Im(func(x+temp[,i]))/eps}
			jac<-matrix(unlist(demo),length(f),n)
			jac[, 1] <- Im(v)/eps
		
		} 
    }
    
    
    return(jac)
    }else stop("indicated method ", method, "not supported.")
    


}
