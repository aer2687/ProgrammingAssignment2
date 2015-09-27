############################################################################################
# Course: R-Programming (Coursera)
# Name: Programming Assignment 2
# Author: Antonio Escobar
# Start Date: 27.09.2015 (DD.MM.YYYY)
# Last Modification: 27.09.2015 (DD.MM.YYYY)
# Content
### 1. makeCacheMatrix(): This function creates a special "matrix" object that can cache its inverse.
### 2. cacheSolve(): This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.  
############################################################################################

### 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
  { 
    # Declare inverse matrix "inv"
    inv <- NULL
    
    # set the value of the matrix
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    # set the value of the matrix
    get <- function() x
    
    # set the value of the inverse
    setinv <- function(inverse) inv <<- inverse
    
    # get the value of the inverse
    getinv <- function() inv
    
    # Return list containing all values
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }
 

### 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.  
cacheSolve <- function(x, ...) 
  { 
    # Get value of inverse from 'x'
    inv <- x$getinv()
    
    # Check if inverse if cached
    if(!is.null(inv)) {
      message("getting cached data")
      # Return cached inverse of 'x' if already calculated
      return(inv)
    }
    
    # Get matrix 'x'
    data <- x$get()
    
    # Calculate, set and return inverse of 'x' 
    message("calculating inverse")
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## Example:
## 1. Create random invertible matrix 
# > mymatrix<-matrix(runif(9,1,100),3,3)

## 2. Create cache matrix using the function makeCacheMatrix()  
# > mycachematrix<-makeCacheMatrix(mymatrix)

## 3. Run cacheSolve() twice 
## Note: the first time the msg "calculating inverse" is displayed; 
# > cacheSolve(mycachematrix)
## the second time the msg "getting cached data" is displayed
# > cacheSolve(mycachematrix) 
