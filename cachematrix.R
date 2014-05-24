### Introduction

## This second programming assignment will require you to write an R
## function that is able to cache potentially time-consuming computations.
## For example, taking the mean of a numeric vector is typically a fast
## operation. However, for a very long vector, it may take too long to
## compute the mean, especially if it has to be computed repeatedly (e.g.
## in a loop). If the contents of a vector are not changing, it may make
## sense to cache the value of the mean so that when we need it again, it
## can be looked up in the cache rather than recomputed. In this
## Programming Assignment you will take advantage of the scoping rules of
## the R language and how they can be manipulated to preserve state inside
## of an R object.

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here). On this assignment we will write a pair of functions that
## cache the inverse of a matrix.

## Computing the inverse of a square matrix can be done with the `solve`
## function in R. For example, if `X` is a square invertible matrix, then
## `solve(X)` returns its inverse.

## For this assignment, assume that the matrix supplied is always
## invertible.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}


#initialize the object
m <- makeCacheMatrix() 
# shows that a is now a list of functions
m       
# shows that a is a list
class(m)       
# shows that the elements of the list are functions
class(m$set)   
# create a test square matrix
A = matrix( 
     c(1, 2, 3, 4),       # the data elements 
     nrow=2,              # number of rows 
     ncol=2,              # number of columns 
     byrow = TRUE)        # fill matrix by rows 
#set the matrix to our object
m$set(A)     
#get the matrix
m$get()      
#calculate the inverse
cacheSolve(m)
#Call it again to get the data from the cache
cacheSolve(m)
