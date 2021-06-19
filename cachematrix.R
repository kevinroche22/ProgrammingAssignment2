## These functions cache the inverse of a matrix while keeping the amount of
## computational power used to a minimum.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL # Specify that object is absent
        set <- function(y) { # Closure fn that access var's from parent fn
                x <<- y
                inverse <<- NULL
        }
        get <- function() x # Retrieves x
        setInverse <- function(solveMatrix) inverse <<- solveMatrix # Sets value of inverse
        getInverse <- function() inverse # Gets value of inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cacheSolve should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) { # Check if inverse has already been calculated
                print("Retrieving from cache")
                return(inv)
        }
        matrixToInv <- x$get()
        inverse <- solve(matrixToInv, ...)
        x$setInverse(inverse)
        inverse   
}


## Test function
kevsMatrix <- makeCacheMatrix(matrix(1:4, 2, 2)) # Run fn and save in kevsMatrix var
kevsMatrix$get() # Access matrix - success
cacheSolve(kevsMatrix) # Solve inverse of matrix - success


## Test function again
kevsMatrix$set(matrix(c(2, 2, 1, 4), 2, 2)) # Set new matrix in kevsMatrix
kevsMatrix$get() # Access matrix - success
cacheSolve(kevsMatrix) # Solve inverse of matrix - success
