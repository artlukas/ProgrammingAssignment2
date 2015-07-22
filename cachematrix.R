## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      # function that changes the matrix stored in the main function
      set <- function(y) {
         x <<- y
         m <<- NULL
      }
      # function that returns the matrix x stored in the main function
      get <- function() x
      # function that stores the value of the input in a variable m in the main function
      setsolve <- function(solve) m <<- solve
      # function that returns the value of m
      getsolve <- function() m
      # list on functions
      list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   
   # check if the value m already exists
   m <- x$getsolve()
   if(!is.null(m)) {
      message("getting cached data")
      # if m exists, return m and exit
      return(m)
   }
   # if m did not exist, create inverse of matrix and store it
   data <- x$get()
   m <- solve(data, ...)
   x$setsolve(m)
   m
}