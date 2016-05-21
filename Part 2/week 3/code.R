nx<-5
ny<-7
X <- cbind(rep(1,nx + ny),rep(c(0,1),c(nx, ny)))
crossprod(X)[1,1]
crossprod(X)
