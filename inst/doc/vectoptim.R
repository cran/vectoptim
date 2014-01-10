### R code from vignette source 'vectoptim.Rnw'

###################################################
### code chunk number 1: vectoptim.Rnw:32-34 (eval = FALSE)
###################################################
## for (s in snP)
##   vF[s] <- OF1(mP[, s])


###################################################
### code chunk number 2: vectoptim.Rnw:39-40 (eval = FALSE)
###################################################
## vF<-as.double(foreach(s=seq(snP)) %dopar% OF1(mP[, s]))


###################################################
### code chunk number 3: vectoptim.Rnw:205-206 (eval = FALSE)
###################################################
## vF<-as.double(foreach(s=seq(snP)) %dopar% OF1(mP[, s]))


###################################################
### code chunk number 4: vectoptim.Rnw:212-221 (eval = FALSE)
###################################################
## for(k in 1:r)
## {
## 	f1 <- func(x+(i==(1:p))*h, ...)
## 	f2 <- func(x-(i==(1:p))*h, ...) 
## 	Daprox[,k] <- (f1 - f2)  / (2*h[i])
## 	Haprox[,k] <- (f1-2*f0+f2)/ h[i]^2
## 	h <- h/v
## 	NULL
## }


###################################################
### code chunk number 5: vectoptim.Rnw:225-227 (eval = FALSE)
###################################################
## DEopt.vectorized(OF, algo = list(), vectorized = FALSE, foreach.option = list(methods = "doMC", nodes = 2))
## 


