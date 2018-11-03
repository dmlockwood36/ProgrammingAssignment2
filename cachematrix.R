## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL
  set <- function(y) {                          ##Set the value of the matrix
    x <<- y 
    inv <<- NULL 
  } 
  get <- function() x                           ##Get the value of the matrix         
  setinv <- function(inverse) inv <<- inverse   ##Set the value of the inverse matrix
  getinv <- function() inv                      ##Get the value of the inverse matrix
  list(
    set = set, 
    get = get, 
    setinv = setinv, 
    getinverse = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  ##return cashed inverse matrix if inverse matrix is not null
  inv <- x$getInverse() 
  if(!is.null(inv)) {                      
    message("getting cache data")   
    return(inv)                            
  }
  Data <- x$get()                 ##if the inverse matrix is null then        
  inv <- solve(Data, ...)         ## solve for the inverse matrix   
  x$setinv(inv)                          
  return(inv)                               
  
} 
