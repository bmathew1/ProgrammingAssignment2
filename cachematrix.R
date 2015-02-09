## These functions allow caching inverse of a matrix after it is run the first time
#  allowing it to be reused again without repeating the inverse operation (resource intensive).

# Description
# makeCacheMatrix encapsulates the functionality for caching the inverse of a given
# matrix x. It returns a list object with four functions which is meant to be used 
# with cacheSolve function to cache inverse of matrix x. The functions returned
# are to 1) set the value of the matrix, 2) get the value of the matrix, 3) set the value 
# of the inverse of matrix x and to 4) get the value of the inverse. The functions must be used
# to operate on the variables (the matrix passed in or the the inverse to be computed)
#
# Usgage
#       makeCacheMatrix(x)
# Arguments
#       x - a (square) matrix that is invertible
# Value 
#       A list with four functions to 1) set the value of the matrix, 2) get the value 
#       of the matrix, 3) set the value of the inverse of matrix x and to 
#       4) get the value of the inverse.
# Note
#       If matrix x is not invertible an error will be thrown
# Example
#       y <- makeCacheMatrix (x)
#       inverseOfx <- cacheSolve(y)

makeCacheMatrix <- function(x = matrix()) {
        # inv - the variable to store the inverse of the matrix
        # set inv to NULL as call to this function is likley to 
        # pass in a new matrix for which the inverse has not been computed yet
        inv <- NULL 
        # set function can be used as an alternative to calling makeCacheMatrix to 
        # set the value of the matrix
        set <- function(y) {
                x <<- y # set the value of the matrix with the matrix passed in
                inv <<- NULL # set the inverse to NULL as the inverse is not computed at this point
        }
        get <- function() x # returns the matrix
        setInverse <- function(solve) inv <<- solve # computes the inverse of matrix x using solve function
        getInverse <- function() inv # return the inverse of the matrix
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse) # return the list of functions to set and get the matrix and the inverse
}

# Description
# cacheSolve returns the inverse of the matrix passed to makeCacheMatrix. First invocation 
# of cacheSolve will execute the resource intensive operation of finding the inverse and 
# cache it. Subsequent call to cacheSolve with the same (unchanged) y will cause the 
# inverse to be returned from the cache
# 
# Usgage
#       cacheSolve(y, ...)
# Arguments
#       y - the object returned by makeCacheMatrix
#       ... - optional arguments to solve function which computes the inverse of matrix
# Value
#       Inverse of the matrix passed into makeCacheMatrix in the call prior to cacheSolve
# Note
#       If the matrix passed to makeCacheMatrix is not invertible, and error will be thrown. If
#       the matrix x passed into makeCacheMatrix is altered by any method other than through a call
#       of the functions returned by makeCacheMatrix, the behaviour descibed cannot be guaranteed.
# Example
#       y <- makeCacheMatrix (x)
#       inverseOfx <- cacheSolve(y) 

cacheSolve <- function(x, ...) {
        # get the inverse by using the getInverse function from the  special matrix object 
        # returned by makeCacheMatrix
        inv <- x$getInverse()
        # if inverse is not null, return it. In other words, if the inverse has already been 
        # computed, return it from th cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # the flow reaches here if the inverse is null, or has not been computed previously.
        data <- x$get() # get the matrix from the makeCacheMatrix object
        inv <- solve(data, ...) # compute the inverse of the matrix from the makeCacheMatrix object
        x$setInverse(inv) # set the inverse that is computed as the the inv value in makeCacheMatrix object
        inv # return the inverse
}
