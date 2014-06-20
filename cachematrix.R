
## these functions will cache the inverse of a matrix in a different environment 
##to save on computation costs

## This function creates a matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
      invM <- NULL
      ## this sets the value of the matrix
      set <- function(y){
            x<<-y
            invM <<- NULL
      }
      ## get value of matrix
      get <- function() x
      ## set the inverse
      setinverse <- function(solve) invM <<- solve
      ## get the inverse
      getinverse <- function() invM
      ## return list of functions
      list(set = set, get = get, setinverse = setinverse, 
           getinverse = getinverse)
}


## This function computes the inverse of the special matrix if it hasn't
## already been calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invM <- x$getinverse()
      ## if inverse is already cached, return it and end
      if(!is.null(invM)){
            message("getting cached data")
            return(invM)
      }
      ## otherwise - get the data
      data <- x$get()
      ##compute the inverse
      invM <- solve(data, ...)
      ##cache the inverse
      x$setinverse(invM)
      invM
}
