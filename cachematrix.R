makeCacheMatrix <- function(mx = matrix()) {
# Stores a matrix and computes the inverse. Both the matrix and inverse are 
#  stored in local variable (i.e., cached) for subsequent retrieval.
# 
# Args: 
#   mx:  a matrix object
# 
# Returns: 
#   A list object of two pairs of functions. 
#   The first pair of functions stores (and retreives) the matrix
#   The second pair of functions computes & stores (and retrieves) the inverse 
#   of the matrix. 
#
# The functions are accessed using the '$' operator

    invMx <- NULL
    
    set <- function(y) {
        mx <<- y  # cache the matrix in mx
        invMx <<- NULL  # Clear any exisiting inversed matrix from the cache
    }
    
    get <- function() mx 
    
    # Cache the argument in invMx; does not actually compute the inverse 
    setSolve <- function(solved) invMx <<- solved 
  
    getSolve <- function() invMx
    
    # Return a vector of the four functions
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

cacheSolve <- function(x) {
# Computes (or retrieves) the inverse of the matrix stored in x$get().
# 
# Args:
#   x: a list object, created by makeCacheMatrix
#
# Returns:
#   The inverse of the matrix cached in x$get().
#   A message is displayed to indicate whether the inverse was retrieved from 
#     the cache, or computed and stored in the cache

    m <- x$getSolve()  # Retrieve the cached inverse matrix
    
    if(!is.null(m)){   # If there is a value in the cache, return it 
        message("cached data available; retreiving...")
        return(m)
    }
    
    # Otherwise, compute the inverse matrix, and store the result in the cache
    data <- x$get()
    message("no data cached; computing and storing inverse...")
    m <- solve(data)
    x$setSolve(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}

### Demonstration #############################################################
## First build the function list 
# > demo <- makeCacheMatrix(matrix(1:4, 2, 2))

## Confirm the 2x2 matrix was created and that no inverse exists
# > demo$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# > demo$getSolve()
# NULL

## Compute and store the inverse, using the cacheSolve function. Subsequent 
## calls of cacheSolve retrieve the solution from the cache vs. recalculating.
# > cacheSolve(demo)
# no data cached; computing and storing inverse...
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# > cacheSolve(demo)
# cached data available; retreiving...
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5