## There are 2 functions here that help identify and solve for an invertible matrix.
## makeCacheMatrix and cacheSolve

## Assign the variable x as a sqaure invertible matrix.  The variable must be square at the minimum in order
## solve for the inversion.  The makeCachefunction will then "cache" the matrices' inverse.

makeCacheMatrix <- function(x = matrix()) {
inv = null
set = function(y){
        ##Allows for solving within a different environment than the current.
        x<<-y
        inv<<-null
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The cacheSolve function will return the inverse of the function defined within makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  ##Identify whether or not the inverse has already been calculated.
  if (!is.null(inv)){ 
    return(inv)
        }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  return(inv)
  ##Returns the value of the inverse of the cache
        } 
}
