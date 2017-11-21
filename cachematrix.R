## Functions for Coursera JHU Data Science R Programming Week 3 assignment 
## to create a demonstrating cache of data. First make CacheMatrix creates 
## a special object that can store and set a matrix and its inverse. cacheSolve
## communicates with makeCacheMatrix to solve a given matrix. If that matrix's 
## inverse is already solved and in cache, cacheSolve returns the cached value
## instead of re-computing.

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function (x=matrix())
{
  i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}

## Return a matrix that is the inverse of 'x'. First checks for cached data.
  cacheSolve <- function(x,...)
  {
    i <- x$getinv()
    if(!is.null(i))
    {
      message("getting cached data")
      return(i)
    }
    matrix <- x$get()
    i <- solve(matrix,...)
    x$setinv(i)
    i
  }
