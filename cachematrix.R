

## This script contains two functions that are used to create a special
## 'matrix' object that stores a matrix and caches its inverse. Please note
## that the structure of the code was provided in the Assignment example by 
## Dr. Peng on "Coursera - R Programming" website. Minor changes were required 
## to modify the example code to achieve the assignment objectives. 

## The first function, `makeCacheMatrix` creates a special "matrix" 
## object that can cache its inverse. The special matrix object is really a 
## list of functions. The first, 'get' simply returns the matrix 'x' from the
## main function. 'Set' changes the matrix, if necessary.
## Setinverse is a function that stores the value of the input for the matrix inversion 
## to the variable 'n' in the main function. It does not actually calculate the inverse.
## Getinverse returns the value of 'n'.

makeCacheMatrix <- function(x = matrix()) {
			## Return a matrix 'x'
			## Set 'n' to null, in case there is an old value
			n <- NULL
			set <- function(y = matrix()) {
					x <<- y
					n <<- NULL
			}
			get <- function() x
			setinverse <- function(solve) n <<- solve
			getinverse <- function() n
			
			## create a list, which is really a series of functions to set and get 
			## the value of the matrix, and set and get the value of the inverse of 
			## the matrix
			
			list(set = set, get = get,
					setinverse = setinverse,
					getinverse = getinverse)
}


## The following function calculates the inverse of the special 'matrix'
## created in "makeCacheMatrix". First, the function checks to see if the 
## inverse has already been calculated. If the inverse exists, the function 
## gets the value from the cache and skips the calculation of the inverse. 
## If the inverse does not exist in the cache, then the function calculates 
## the inverse using the 'solve function and sets the value of the matrix 
## inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Pass matrix 'x' to new function with arguments
        n <- x$getinverse()
        
        ## checks if 'n' exists and is not NULL. If there is a stored value
        ## for the inverse of the matrix, it is returned.
        if(!is.null(n)) {
        		message("getting cached matrix inverse")
        		return(n)
        }
        
        ## the matrix from 'makeCacheMatrix' is assigned to a new variable
        ## 'matrix_inverse'
        matrix_inverse <- x$get()
        
        ## solves for matrix inverse and stores it
        n <- solve(matrix_inverse, ...)
        x$setinverse(n)
        n
}
