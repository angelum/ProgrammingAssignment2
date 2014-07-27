## A set of 2 functions used to calculate the inverse of a matrix
## If the value was previously calculated it is read from cache 

## Function 1
## Creates a special matrix object which is a list 
## containing a function to:
## 1) set and get the value of the matrix (m)
## 2) set and get the inverse of the matrix (inv)

makeCacheMatrix <- function(m = matrix()) {
      inv <- NULL
      
      ## Set the value of the matrix
      set <- function( y ) {
            m <<- y
            inv <<- NULL
      }
      
      ## Get the value of the matrix
      get <- function() m
      
      ## Set the inverse of the matrix
      setInverse <- function(inverse) inv <<- inverse
            
      ## Get the inverse of the matrix
      getInverse <- function() inv
      
      ## Return a list of 4 objects
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## Function 2
## Calculates the inverse of the special matrix created with
## Function 1 (makeCacheMatrix). If the inverse value has been already
## calculated then it returns the value from cache; otherwise it
## calculates the inverse value of the matrix and stores it in cache

cacheSolve <- function(makeCacheMatrix, ...) {
        
      ## Get the value of the inverse of matrix m
      inv <- makeCacheMatrix$getInverse()
      
      ## Return the inverse from cache if it was previously calculated
      if( !is.null(inv) ) {
            message("getting cached data")
            return(inv)
      }
      
      ## Get the matriX
      data <- makeCacheMatrix$get()
      
      ## Calculate the inverse using function solve
      inv <- solve(data, ...)
      
      ## Set and return the inverse matrix of m
      makeCacheMatrix$setInverse(inv)
      inv
}
