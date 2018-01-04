## Exercises 1

# 1
(1:20)
(20:1)
c(1:20, 19:1)
tmp <- c(4,6,3)
rep(tmp, 10)
rep(tmp,l=31)
rep(tmp,times=c(10,20,30))

# 2
tmp <- seq(3,6,by=0.1)
exp(tmp)*cos(tmp)


# 3
(0.1^seq(3,36,by=3))*(0.2^seq(1,34,by=3))
(2^(1:25))/(1:25)

# 4
tmp <- 10:100
sum(tmp^3+4*tmp^2)

tmp <- 1:25
sum((2^tmp)/tmp + 3^tmp/(tmp^2))


# 5
paste("label", 1:30)
paste("fn", 1:30,sep="")

# 6
set.seed(50)
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)

yVec[-1] - xVec[-length(xVec)]
sin(yVec[-length(yVec)]) / cos(xVec[-1])

xVec[-c(249,250)] + 2*xVec[-c(1,250)]-xVec[-c(1,2)]
# for an answer which works whatever the length of xVec,
xVecLen <- length(xVec)
xVec[-c(xVecLen-1,xVecLen)] + 2*xVec[-c(1,xVecLen)] - xVec[-c(1,2)]

sum(exp(-xVec[-1]) / (xVec[-length(xVec)]+10))

# 7 
yVec[yVec>600]
(1:length(yVec))[yVec>600] 
xVec[yVec>600]
sqrt(abs(xVec-mean(xVec)))
sum( yVec>max(yVec)-200 )
sum(xVec%%2==0)
xVec[order(yVec)]
yVec[c(T,F,F)]


# 8
1+sum(cumprod(seq(2,38,b=2)/seq(3,39,b=2)))



# Exercises 2

# 1

( tmp <- matrix( c(1,5,-2,1,2,-1,3,6,-3),nrow = 3) )
tmp %*% tmp %*% tmp

tmp[,3] <- tmp[,2]+tmp[,3]


# 2
tmp <- matrix(c(10,-10,10), b=T, nc=3, nr=15)
#t(tmp)%*%tmp
crossprod(tmp)


# 3
matE <- matrix(0,nr=6,nc=6)
matE[ abs(col(matE)-row(matE))==1 ] <- 1


# 4
outer(0:4,0:4,"+")


# 5
outer(0:4,0:4,"+")%%5
outer(0:9,0:9,"+")%%10
outer(0:8,0:8,"-")%%9

# 6
yVec <- c(7,-1,-3,5,17)
AMat <- matrix(0,nr=5, nc=5)
AMat <- abs(col(AMat)-row(AMat))+1
AMat
solve(AMat)%*%yVec
solve(AMat,yVec)
solve(AMat,matrix(yVec,nc=1) )


# 7
set.seed(75)
aMat <- matrix( sample(10, size=60, replace=T), nr=6)
aMat

apply(aMat, 1, function(x){sum(x>4)})
which( apply(aMat,1,function(x){sum(x==7)==2}) )

aMatColSums <- colSums(aMat)
cbind( rep(1:10,rep(10,10)), rep(1:10,10) ) [outer(aMatColSums,aMatColSums,"+")>75,]

aMatColSums <- colSums(aMat)
which( outer(aMatColSums,aMatColSums,"+")>75, arr.ind=T )


# 8
sum( (1:20)^4 ) * sum( 1/(4:8) ) 
sum(outer((1:20)^4,4:8,"/"))

sum( (1:20)^4 / (3 + outer(1:20,1:5,"*")))

sum( outer(1:10,1:10,function(i,j){ (i>=j)*i^4/(3+i*j) }) )




# Exercises 3

# 1
  # a
tmpFn1 <- function(xVec)
{
  xVec^(1:length(xVec))
}
tmpFn2 <- function(xVec)
{
  n <- length(xVec)
  (xVec^(1:n))/(1:n)
}
  

  # b
tmpFn3 <- function(x, n)
{
  1 + sum((x^(1:n))/(1:n))
}

  #try
tmpFn1(1:3)
tmpFn2(1:3)
tmpFn3(3,4)



# 2
tmpFn -> function(xVec)
{
  n <- length(xVec)
  ( xVec[ -c(n-1,n) ] + xVec[ -c(1,n) ] + xVec[ -c(1,2) ] )/3
}
tmpFn <- function(xVec)
{
  n <- length(xVec)
  ( x[1:(n-2)] + x[2:(n-1)] + x[3:n] )/3
}
tmpFn( c(1:5,6:1) )

# 3
tmpFn <- function(x)
{
  ifelse(x < 0, x^2 + 2*x + 3, ifelse(x < 2, x+3, x^2 + 4*x - 7))
}
tmp <- seq(-3, 3, len=100)
plot(tmp, tmpFn(tmp), type="l")


# 4
tmpFn <- function(mat)
{
  mat[mat%%2 == 1] <- 2 * mat[mat%%2 == 1]
  mat
}


# 5

#For the specific case of n = 5 and k = 2:
tmp <- diag(2, nr = 5)
tmp[abs(row(tmp) - col(tmp)) == 1] <- 1
tmp

#for the function for the general case:
tmpFn <- function(n, k)
{
  tmp <- diag(k, nr = n)
  tmp[abs(row(tmp) - col(tmp)) == 1] <- 1
  tmp
}


# 6
quadrant <- function(alpha)
{
  1 + (alpha%%360)%/%90
}

quadrant2 <- function(alpha)
{
  floor(alpha/90)%%4 + 1
}



# 7
weekday <- function(day, month, year)
{
  month <- month - 2
  if(month <= 0) {
    month <- month + 12
    year <- year - 1
  }
  cc <- year %/% 100
  year <- year %% 100
  tmp <- floor(2.6*month - 0.2) + day + year + year %/% 4 + cc %/% 4 - 2 * cc
  c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[1+tmp%%7]
}
c( weekday(27,2,1997), weekday(18,2,1940), weekday(21,1,1963) )


weekday2 <- function(day, month, year)
{
  flag <- month <= 2
  month <- month - 2 + 12*flag
  year <- year - flag
  cc <- year %/% 100
  year <- year %% 100
  tmp <- floor(2.6*month - 0.2) + day + year + year %/% 4 + cc %/% 4 - 2 * cc
  c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[1+tmp%%7]
}



# 8
  # a
testLoop <- function(n)
{
  xVec <- rep(NA, n-1)
  xVec[1] <- 1
  xVec[2] <- 2
  for( j in 3:(n-1) )
    xVec[j] <- xVec[j-1] + 2/xVec[j-1]
  xVec
}
testLoop(10)

  # b
testLoop2 <- function(yVec)
{
  n <- length(yVec)
  sum( exp(1:n) )
}



# 9
  # a
quadmap <- function(start, rho, niter)
{
  xVec <- rep(NA,niter)
  xVec[1] <- start
  for(i in 1:(niter-1)) {
    xVec[i + 1] <- rho * xVec[i] * (1 - xVec[i])
  }
  x
}

  # b
quad2 <- function(start, rho, eps = 0.02)
{
  x1 <- start
  x2 <- rho*x1*(1 - x1)
  niter <- 1
  while(abs(x1 - x2) >= eps) {
    x1 <- x2
    x2 <- rho*x1*(1 - x1)
    niter <- niter + 1
  }
  niter
}


# 10

  # a
tmpAcf <- function(xVec)
{
  xc <- xVec - mean(xVec)
  denom <- sum(xc^2)
  n <- length(x)
  r1 <- sum( xc[2:n] * xc[1:(n-1)] )/denom
  r2 <- sum( xc[3:n] * xc[1:(n-2)] )/denom
  list(r1 = r1, r2 = r2)
}


  # b
tmpAcf <- function(x, k)
{
  xc <- x - mean(x)
  denom <- sum(xc^2)
  n <- length(x)
  tmpFn <- function(j){ sum( xc[(j+1):n] * xc[1:(n-j)] )/denom }
  c(1, sapply(1:k, tmpFn))
}


# Exercises 4

# 1
fun4q1a <- function(xVec, yVec){
  colSums( outer(yVec, xVec, "<") )
}

fun4q1b <- function(xVec, yVec){
  rowSums( sapply(yVec, FUN=function(y){y < xVec}) )
}

fun4q1c <- function(xVec, yVec){
  rowSums( vapply(yVec, FUN=function(y){y<xVec}, FUN.VALUE=seq(along=xVec)) )
}

fun4q1d <- function(xVec,yVec)
{
  leny <- length(yVec)
  mat <- matrix(rep(xVec,leny), byrow=T, nrow=leny)
  apply( yVec<mat, 2, sum )
}

# Both fun4q1b and fun4q1d fail if either xVec or yVec has length 0
# Both fun4q1d and fun4q1a fail if xVec and yVec are matrices. 


rjr1 <- rnorm(10000)
rjr2 <- rnorm(12000)
system.time(fun4q1a(rjr1,rjr2))
system.time(fun4q1b(rjr1,rjr2))
system.time(fun4q1c(rjr1,rjr2))
system.time(fun4q1d(rjr1,rjr2))


# 2
tmpFn <- function(mat){
  mat[, !apply(is.na(mat), 2, any), drop = F]
}

tmpFn2 <- function(mat){
  mat[!apply(is.na(mat), 1, any), !apply(is.na(mat), 2, any), drop = F]
}


# 3

 # a
empCopula <- function( u, v, xVec, yVec )
{
  n <- length(xVec)
  rVecN <- rank(xVec)/(n+1)
  sVecN <- rank(yVec)/(n+1)
  sum( (rVecN <= u) & (sVecN <= v) ) /n
}


 # b
 # first solution: using outer
empCopula2 <- function( u, v, xVec, yVec )
{
  n <- length(xVec)
  rVecN <- rank(xVec)/(n+1)
  sVecN <- rank(yVec)/(n+1)
  valuesN <- colSums( outer(rVecN, u, "<=")&outer(sVecN, v, "<=") )
  cbind( uCoord = u, vCoord = v, empCop=valuesN/n )
}

 # second solution: using apply 
empCopula3 <- function( u, v, xVec, yVec )
{
  n <- length(xVec)
  rVecN <- rank(xVec)/(n+1)
  sVecN <- rank(yVec)/(n+1)
  tempf <- function(uv){
    sum( (rVecN <= uv[1]) * (sVecN <= uv[2]) )
  }
  valuesN <- apply( cbind(u,v), 1, tempf )
  cbind( uCoord = u, vCoord = v, empCop=valuesN/n )
}
 
 # third solution: using mapply
empCopula4 <- function( u, v, xVec, yVec )
{
  n <- length(xVec)
  rVecN <- rank(xVec)/(n+1)
  sVecN <- rank(yVec)/(n+1)
  valuesN <- mapply( FUN=function(u1,v1){ sum((rVecN<=u1)*(sVecN<=v1)) }, u, v )
  cbind( uCoord = u, vCoord = v, empCop=valuesN/n )
}



# 4
 # a
funA <- function (n)
{
  su <- 0
  for(r in 1:n)
  {
    for(s in 1:r)
      su <- su+s^2/(10+4*r^3)
  }
  su
}

 # b
funB <- function (n)
{
  mat <- matrix(0, ncol=n, nrow=n)
  sum( (col(mat)^2)/(10+4*row(mat)^3)*(col(mat)<=row(mat)) )
}

 # c
funC <- function (n)
{
  sum( outer(1:n,1:n,FUN=function(r,s){ (s<=r)*(s^2)/(10+4*r^3) }) )
}

 # d
funD <- function (n)
{
  tmpfn <- function(r){sum(((1:r)^2)/(10+4*r^3))}
  sum(sapply(1:n, FUN=tmpfn))
}

 # e
funE <- function (n)
{
  tmpfn <- function(r){sum(((1:r)^2)/(10+4*r^3))}
  sum(unlist(lapply(1:n, FUN=tmpfn)))
}


funF <- function (n)
{
  tmpf <- function(s,r){(s^2)/(10+4*r^3)*(s<=r)}
  sum(mapply(tmpf, rep(1:n, times=rep(n,n)), 1:n))
}


# 5
 # a
 # solution 1:
queue1 <- function(n, aRate, sRate)
{
  w <- 0
  for(i in 1:n){
    w <- max(0, w+rexp(1,sRate)-rexp(1,aRate))
  }
  w
}
 # solution 2:
queue2 <- function(n, aRate, sRate)
{
  w <- 0
  s <- rexp(n, sRate)
  a <- rexp(n, aRate)
  for(i in 1:n){
    w <- max(0, w+s[i]-a[i])
  }
  w
}

# b
queueRep1 <- function (nReps, n, aRate, sRate)
{
  wVec <- rep(NA, nReps)
  for(j in 1:nReps)
    wVec[j] <- queue2(n, aRate, sRate)
  wVec
}
queueRep2 <- function (nReps, n, aRate, sRate)
{
  sapply( rep(n,nReps), queue2, aRate, sRate )
}
# or
replicate(nReps, queue2(n,aRate,sRate))


 # c
queueRep3 <- function (nReps, n, aRate, sRate)
{
  w <- rep(0, nReps)
  s <- matrix(rexp(n*nReps, sRate), ncol=nReps)
  a <- matrix(rexp(n*nReps, aRate), ncol=nReps)
  for(i in 1:n){
    w <- pmax(0, w+s[i,]-a[i,])
  }
  w
}


# 6
 # a
rwalk <- function(n)
{
  c( 0, cumsum(sample( c(-1,1), n, replace=TRUE, prob=c(0.5,0.5))) )
}

 # b
rwalkPos <- function(n)
{
  rw <- cumsum(c(0, sample( c(-1,1), n, replace=TRUE, prob=c(0.5,0.5))))
  sum( (rw[-(n+1)] + rw[-1]) > 0 )
}

 # c
rwalkPos1 <- function(nReps, n)
{
  results <- rep(NA, nReps)
  for(i in 1:nReps)
    results[i]<-rwalkPos(n)
  results
}
rwalkPos2 <- function(nReps, n)
{
  replicate( nReps, rwalkPos(n) )
}


# d
rwalkPos3 <- function(nReps, n)
{
  stepWalks <- matrix( sample( c(-1,1), n, replace=TRUE, prob=c(0.5,0.5)), nr=nReps )
  for(j in 2:n)
    stepWalks[,j] <- stepWalks[,j] + stepWalks[,j-1]
  stepWalks <- cbind(0, stepWalks)
  rowSums( stepWalks[,1:n] + stepWalks[,2:(n+1)]>0 )
}


# Exercises 5

# 1
 # a
tsEwma <- function( tsDat, m0=0, delta=0.7)
{
  n <- length(tsDat)
  mVec <- rep(NA,n+1)
  mVec[1] <- m0
  for(j in 2:(n+1)){
    mVec[j] <- (1-delta)*tsDat[j-1] + delta*mVec[j-1]
  }
  ts(mVec[-1], start=start(tsDat), frequency=frequency(tsDat))
}

 # b
tsEwma2 <- function( tsDat, m0=0, delta=0.7)
{
  tsPars <- tsp(tsDat)
  tsDat <- c(tsDat)
  n <- length(tsDat)
  mVec <- rep(NA,n+1)
  mVec[1] <- m0
  for(j in 2:(n+1)){
    mVec[j] <- (1-delta)*tsDat[j-1] + delta*mVec[j-1]
  }
  ts(mVec[-1], start=tsPars[1], frequency=tsPars[3])
}

# testing:
tmp <- ts(rnorm(400000), start=c(1960,3), frequency=12)
system.time(tsEwma2(tmp))
system.time(tsEwma(tmp))


# 2
 # a
myListFn <- function(n)
{
  xVec <- rnorm(n)
  xBar <- mean(xVec)
  yVec <- sign(xBar)*rexp(n, rate=abs(1/xBar))
  count <- sum( abs(yVec) > abs(xVec) )
  list(xVec=xVec, yVec=yVec, count=count)
}

 # b
lapply( rep (10,4), myListFn)
sapply( rep (10,4), myListFn)

myList <- lapply( rep(10,4), myListFn )
myMatrix <- sapply( rep(10,4), myListFn )

 # c
myList <- lapply( rep(10,1000), myListFn )
lapply(myList, FUN=function(x){x[[2]]})
lapply(myList, FUN="[[", 2)
lapply(myList, FUN="[[", "yVec")

 # d
sapply(myList, FUN="[[", 2)
vapply(myList, FUN="[[", FUN.VALUE=rep(0,10), 2)
sapply(myList, FUN=function(x){x[[2]]})
vapply(myList, FUN=function(x){x[[2]]}, FUN.VALUE=rep(0,10))
sapply(mList, FUN="[[", "yVec")
vapply(myList, FUN="[[", FUN.VALUE=rep(0,10), "yVec")

 # e
myList2 <- lapply(myList, function(x){list(xVec=x$xVec, yVec=x$yVec)})

 # f
which( unlist( lapply(myList, function(x){x[[3]]>2}) ) ) # pick out the indices of those lists which satisfy the  conditionn  

myList[which( unlist(lapply( myList, function(x){x[[3]]>2} )) )]



# 3
 # a
partA <- sapply(myList, function(x){ sum(x$xVec*(1:10))/sum(x$yVec*(1:10)) })

 # b
 # solution 1:
myMat <- t(sapply( myList, function(x){x$xVec-x$yVec}))
 # solution 2:
myMat2 <- matrix( unlist( lapply(myList, FUN="[[",1) ) -
                    unlist( lapply(myList, FUN="[[",2) ), nc=10, by=T )
 # solution 3:
myMat3 <- matrix( unlist(lapply(myList, function(x){x$xVec-x$yVec})), nc=10, by=T )


 # c

 # solution 1: (quick)
sum(sapply(myList, function(x){x$xVec[2]})*(1:1000)) /
  sum(sapply(myList, function(x){x$yVec[2]})*sapply(myList, function(x){x$count})) 

 # solution 2:
myDf <- data.frame(myList)
myDf[2, seq(1,3000,by=3)]

myMat <- as.matrix(data.frame(myList))
names(myMat) <- NULL
sum((1:1000) * myMat[2,seq(1,3000,by=3)])/
  sum(myMat[2, seq(3,3000,by=3)] * myMat[2, seq(2,3000,by=3)])

## The last line could be replaced by
sum( (1:1000) * myMat[2,c(T,F,F)] )/sum( myMat[2,c(F,F,T)] * myMat[2,c(F,T,F)] )






# 4
 # a
testFn2 <- function(xArray)
{
  wArray <- sweep(testArray, c(2,3), apply(testArray, c(2,3), min))
  zArray <- apply(testArray, c(2,3), FUN=function(x){ sum(x) - max(x)})
  list(wArray=wArray, zArray=zArray)
}
 
 # b
testFn <- function( xArray)
{
  apply( apply(xArray, c(1,2), FUN=function(x){x^(1:length(x))}), c(3,1), sum )
}



# 5

 # a
shift <- function(X,a,b)
{
  X[,1] <- X[,1] + a
  X[,2] <- X[,2] + b
  X
}

 # b
rotate <- function(X,r){
  X%*%matrix(c(cos(r), -sin(r), sin(r), cos(r)), nrow = 2)
}
 
A <- cbind(c(0,1,2,4/9,14/9), c(0,3,0,4/3,4/3))

 # c
 # solution 1:
arrayA <- array(0, dim=c(5,2,25))
for(i in 1:25){
  arrayA[,,i] <- rotate(A,2*pi*(i-1)/24)
}
 
 # solution 2:
arrayA<-vapply(1:25,
               FUN=function(i){
                 rotate(A,2*pi*(i-1)/24)
               },
               matrix(0,nrow=5, ncol=2)
)
  # 1
plot(c(-10,10), c(-10,10), ann=F, type=’n’)
for(i in 1:25)
  drawA(arrayA[,,i])

  # or
plot(c(-10,10), c(-10,10), ann=F, type=’n’)
invisible(sapply( 1:25, FUN=function(i){ drawA(arrayA[,,i]) } ))

  # 2
plot(arrayA[2,1,], arrayA[2,2,])


  # 3
plot(1:25, arrayA[2,1,])


# d
scale <- function(X,a,b){
  X%*%matrix(c(a,0,0,b), nrow=2)
}
arAscaled <- vapply(1:25,
                    FUN=function(i){
                      scale(arrayA[,,i],2,3)
                    },
                    matrix(0,nrow=5, ncol=2)
)
plot(c(-10,10), c(-10,10), ann=F, type='n')
invisible(sapply( 1:25, FUN=function(i){ drawA(arrayA[,,i]) } ))
invisible(sapply( 1:25, FUN=function(i){ drawA(arAscaled[,,i]) } ))


# e
arArandom <- array(0, dim=c(5,2,25))
arArandom[,,1] <- A

for(i in 2:25){
  arArandom[,,i] <-
    shift(
      rotate(
        scale(arArandom[,,i-1], runif(1,0.5,1.5),runif(1,0.5,1.5)),
        2*pi*runif(1,-1,1)
      ),
      runif(1,-1,1), runif(1,-1,1)
    )
}
  ## create an animation
oopt = ani.options(interval = 0.2, nmax = 25)
for (i in 1:ani.options("nmax")){
  plot(c(-10,10), c(-10,10), ann=F, type=’n’)
  drawA(arArandom[,,i])
  ani.pause()
}















