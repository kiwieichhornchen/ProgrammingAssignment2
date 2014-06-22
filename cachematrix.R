## The following work together to calculate an inverse of a matrix and cache it

## makeCacheMatrix creates a list of functions that
## 1. Writes a matrix into the cache and resets the inverse
## 2. Get the value of the matrix from the cache
## 3. Writes the inverse of a matrix into the cache
## 4. Get the inverse of the matrix from the cache

##Example:
##Firstly create mat, a 2x2 matrix
##mat <- matrix (data = c(1,1,2,1), nrow = 2, ncol =2)

##Then call the function with:
##z  <- makeCacheMatrix(mat)      
##Alternatively, can call explicitly call 'set' via:
##z  <- makeCacheMatrix()
##z$set(mat)

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL                                               #initialize the inverse to NULL for new fnc call
        set <- function(input) {                                   #set the matrix in cache, plus inverse to NULL
                x <<- input
                invx <<- NULL
        }
        get <- function() x                                        #get value of matrix from cache                  
        setinverse <- function(solve_input) invx <<- solve_input   #set inverse of matrix into cache
        getinverse <- function() invx                              #get inverse of matrix from cache
        list(set = set, get = get,                                 #return list of functions
                setinverse = setinverse,
                getinverse = getinverse)   
}


## cacheSolve calculates the inverse of matrix, after first checking the cache for a value.
## If a value is found in the cache, this value is returned and the function terminates

##Example call continuing from above:
##cacheSolve(z)

cacheSolve <- function(z, ...) {
        invx <- z$getinverse()                              # check cache for inverse of matrix
        if(!is.null(invx)) {                                   # if value exists, return value and exit function
                message("getting cached data")
                return(invx)
        }
        x <- z$get()                                           # get matrix from cache 
        invx <- solve(x, ...)                                  # calculate inverse of matrix
        z$setinverse(invx)                                     # store inverse of matrix in cache
        invx                                                   # return inverse of matrix
}
