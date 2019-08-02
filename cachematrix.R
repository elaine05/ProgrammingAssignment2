makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ## this function changes the matrix stored in makeCacheMatrix 
    x <<- y
    m <<- NULL
  }
  get <- function() x ##  returns a matrix x stored in makeCacheMatrix
  setinverse <- function(solve) m <<- solve ## store the value of the input in a variable m
  getinverse <- function() m 
  list(set = set, get = get, ## register these functions 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("loading cached data")
    return(m)
  }
  dat <- x$get()
  m <- solve(dat, ...) ## calculate the inverse
  x$setinverse(m)
  m
}


#-----------TEST--------
t1 <- matrix( c(5, 1, 0,
                3,-1, 2,
                4, 0,-1), nrow=3, byrow=TRUE)
t2 <- makeCacheMatrix(t1)
cacheSolve(t2)