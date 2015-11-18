## Assignment: Caching the Inverse of a Matrix

## First,create a special "matrix", which is really a list containing a function to set the value of the matrix,a function to get the value of the matrix,
 #a function to set the value of the inverse of the matrix and a function to get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
 s<- NULL
 set <- function(y) {
            x <<- y
            s <<- NULL
 }
 get <- function() x
 setinverse <- function(solve) s <<- solve
 getinverse <- function() s
  list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}



##Create a function that calculates the inverse of the special "matrix". First checks to see if the inverse has already been calculated. If so,
 #it gets the mean from the cache and skips the computation. If not, calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
       if(!is.null(s)) {
               message("getting cached data")
               return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
    }
      




# Try an example with: x<-matrix(c(1, 3, 2, 4),nrow=2,ncol=2)


