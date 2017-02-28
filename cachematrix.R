#The makeCacheMatrix function takes a matrix as an argument, and caches the inverse of that matrix, while also providing functions for cacheSolve. 
#The functions, along with the inverted matrix, will be passed to cacheSolve. 

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmat <<- inverse
  getinverse <- function() invmat
  list(set = set, get = get,
        setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve checks if there is a cached inverted matrix from makeCachedMatrix. If there is, it takes this matrix from the cache. If not, it computes an inverted matrix from the matrix passed to makeCacheMatrix. 

cacheSolve <- function(x, ...) {
  invmat <- x$getinverse() ##Check if there is a cached matrix. 
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat) ##If there is a cached matrix, return it. 
  }
  data <- x$get() ##If there isn't a cached matrix, get the original matrix from makeCachedMatrix and, 
  invmat <- solve(data, ...) ##solve it. 
  x$setinverse(invmat)
  invmat
        ## Return a matrix that is the inverse of 'x'
}

##Running the functions:
##testmat <- matrix(1:4, nrow = 2, ncol = 2)
##cachedmat <- makeCacheMatrix(testmat)
##cacheSolve(cachedmat)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(cachedmat)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> 