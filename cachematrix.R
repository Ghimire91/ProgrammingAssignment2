## Put comments here that give an overall description of what your
## functions do

## Thera are two functions makeCacheMatrix and makeCacheMatrix
## makeCacheMatrix consists of set, get, setinv, getinv
## Library(MASS) is used to calculate inverse for non squared as well as square matrices

Library(MASS)
makeCacheMatrix <- function(x = matrix()) {
inv<- NULL ##initializing inverse as NULL
set<- function(y){
  x<<- y
  inv<<- NULL
}
get<- function(x) ## Function to get matrix x
  setinv<- function(inverse) inv<<- inverse
    getinv<- function(){
      inver<-ginv(x)
      inver%*%x ## Function to obtain inverse of the matrix
    }
    list(set = set, get = get
    setinv = setinv,
    getinv = getinv)
}

# This function creates a special "matrix" object that can cache its inverse.
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) #gets cache data
  {
  inv<-x$getinv()
  if(!is.null(inv)){ ##Checking weather inverse is null
    message("getting cached data!")
    return(inv) ##returns inverse value
  }
    data<-x$get()
    inv<- solve(data, ...) ##Calculates inverse value
    x$setinv(inv)
    inv ##return a matrix that is inverse of 'x'
} 
  
