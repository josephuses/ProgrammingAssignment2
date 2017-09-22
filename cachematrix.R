## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix
# The function, makeCacheMatrix, creates a special "matrix", which is really
# a list containing a function to:
# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of the inverse of the matrix using the solve() function
# 4 get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse of the matrix
  set_inverse <- function(solve) m <<- solve
  # get the value of the inverse of the matrix
  get_inverse <- function() m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function
# The following function calculates the inverse of the special "matrix"
# created with the makeCacheMatrix function. However, it first checks to see
# if the inverse has already been calculated. If so, it gets the inverse
# from the cache and skips the computation. Otherwise, it calculates the
# inverse of the data and sets the value of the inverse in the cache via the
# set_inverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # check if the inverse has already been calculated
  m <- x$get_inverse()
  # 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # get the value of the matrix
  data <- x$get()
  # calculate and return the the inverse of the matrix
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}


## The following tests the above functions
# source("cachematrix.R")
# M <- makeCacheMatrix(matrix(runif(36), ncol = 6))
# M$get()
# M$get_inverse()
# cacheSolve(M)
# cacheSolve(M)