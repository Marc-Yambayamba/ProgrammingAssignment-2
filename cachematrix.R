## This function takes a matrix and return functions which allow to set the matrix, to get it
## and to set the inverse of the matrix and to get the inverse. which simplies the fact of typing 
## code every time when we want to use the function.

makeCacheMatrix <- function(x = matrix()) {
  # takes the matrix x and 
  # return: a list containing functions to
  #              1. set the matrix
  #              2. get the matrix
  #              3. set the inverse
  #             4. get the inverse
  # and then create a function which can be used as input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  # here x is the output of the makeCacheMatrix function above and 
  # return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}

