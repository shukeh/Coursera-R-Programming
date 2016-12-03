# The makeCacheMatrix function creates a list containing set of functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinvmatrix <- function(inverse) {
    inv <<-  inverse
  }
  
  getinvmatrix <-function(){
    inv  
  }
  
  list(set=set,get=get,setinvmatrix=setinvmatrix,getinvmatrix=getinvmatrix)
}


# The cacheSolve() function returns the inverse of the matrix.If the inverse of the matrix 
# is already available then it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache using
# setinverse function.


cacheSolve <- function(x, ...) {
  inv <- x$getinvmatrix()
  
  if(!is.null(inv)){
    message("Reading data from cache")
    return(inv)
  }
  
  inv <- solve(x$get())
  x$setinvmatrix(inv)
  inv
  
}


## Sample run:
## > m <-makeCacheMatrix(matrix(c(1,-1/4,-1/4,1),nrow=2,ncol=2))
## > m$get()
## [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## > cacheSolve(m)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## > cacheSolve(m)
## Reading data from cache
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667