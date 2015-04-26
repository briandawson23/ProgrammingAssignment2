## Programmer: Brian Dawson
## Date: 4/24/2015
## Description:
## These two functions will cache the inverse of a matrix by
## first, creating a matrix that can cache its inverse and
## secondly, returning the inverse of the matrix.  If the inverse
## has already been calculated, it will be returned in cache rather
## than calculating the inverse each time, hopefully providing 
## significant performance improvements.


## Programmer: Brian Dawson
## Date: 4/24/2015
## Function: makeCacheMatrix
## Description: Returns a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
           inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            
			setinv <- function(inverse) {
				inv <<- inverse
			}
            
			getinv <- function() {
				inv
			}
			
            list(set = set, get = get,setinv = setinv,getinv = getinv)
			
}

## Programmer: Brian Dawson
## Date: 4/24/2015
## Function: cacheSolve
## Description: Return the inverse of a matrix by cache if already present.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
     cacheInv <- x$getinv()
  
      if(!is.null(cacheInv)) {
          message("Getting cached data")
          return(cacheInv)
      }
      
      data <- x$get()
      
      m <- solve(data, ...)
      
      x$setinv(m)
      
      m
}


testProgram = function(testMat){
      
        temp = makeCacheMatrix(testMat)
        
        start = Sys.time()
        cacheSolve(temp)
        duration = Sys.time() - start
        print(duration)

        start = Sys.time()
        cacheSolve(temp)
        duration = Sys.time() - start
        print(duration)
}
