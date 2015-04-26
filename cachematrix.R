## Function makeCacheMatrix stores the inverse for a matrix
## Function cacheSolve calculates the inevrse of a matrix
## prints the inverse from the cache memory.


## Function to store the inverse.

makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  setmatrix<-function(y=matrix()){
    x<<-y
    inv<<-NULL
  }
  getmatrix<-function()x
  setmatrixinv<-function(solve=matrix())inv<<-solve
  getmatrixinv<-function()inv
  list(setmatrix=setmatrix,getmatrix=getmatrix,
       setmatrixinv=setmatrixinv,getmatrixinv=getmatrixinv)
}



## Function to calculate the inverse of  the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getmatrixinv()
  if(!is.null(inv)){
    message("getting cached matrix inverse")
    return(inv)
  }
  data<-x$getmatrix()
  inv<-solve(data,...)
  x$setmatrixinv(inv)
  inv
}

