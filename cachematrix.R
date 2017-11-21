## Functions for Coursera JHU Data Science R Programming Week 3 assignment 
## to create a demonstrating cache of data. First make CacheMatrix creates 
## a special object that can store and set a matrix and its inverse. cacheSolve
## communicates with makeCacheMatrix to solve a given matrix. If that matrix's 
## inverse is already solved and in cache, cacheSolve returns the cached value
## instead of re-computing.

## Creates a special matrix object that can cache its inverse. Returns a list that
## contains a function to: 1) set the matrix value, 2) get the matrix value,
## 3) set the inverse matrix value, and 4) get the inverse value
makeCacheMatrix <- function (x=matrix())
{
  ## the value of inverse i set to be NULL until the solve function is executed to
  ## replace this value.
  i <- NULL
  
  ## set the value of the matrix
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse
  setinv <- function(solve) i <<- solve
  
  ## get the value of the inverse
  getinv <- function() i
  
  #this function returns the list below, as described earlier
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}

## Return a matrix that is the inverse of 'x'. First checks for cached data.
## If cached inverse data exists for this matrix, cacheSolve returns that inverse
## Otherwise, cacheSolve computes the inverse of the matrix.
  cacheSolve <- function(x,...)
  {
    ## Checks for inverse from makeCacheMatrix structure
    i <- x$getinv()
    
    ## If inverse matrix has been computed, retrieve and return that data
    if(!is.null(i))
    {
      message("getting cached data")
      return(i)
    }
    
    ## Otherwise, compute the inverse and return the computed data
    matrix <- x$get()
    i <- solve(matrix,...)
    x$setinv(i)
    i
  }
