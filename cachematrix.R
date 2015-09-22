# Matrix <- function(x = matrix())
# This function returns a list with 4 functions:
# - a function to set the value of the matrix,
# - a function to get the value of the matrix,
# - a function to set the cached value of the matrix inverse,
# - a function to get the cached value of the matrix inverse.
# Example of use:
# myMatrix <- makeCacheMatrix(matrix(1:4,2,2))
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # setter and getter function for matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    # setter and getter function for inverse
    setInv <- function(inv) inv <<- inv
    getInv <- function() inv
    # return list with the 4 functions
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}


# cacheSolve <- function(x, ...)
# Argument x is a makeCacheMatrix object
# This function checks if the matrix contained in x
# already has a cached inverse. If yes, the function
# returns the cached value. If no, the inverse of the
# matrix is calculated, stored in the cache, and it
# is returned.
# Remark: it is assumed that the inverse of the matrix
# exists.
# Example of use:
# cacheSolve(myMatrix)
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    # if cached inverse exists, return it
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    # else calculate the inverse and cache it
    m <- x$get()
    inv <- solve(m)
    x$setInv(inv)
    # return calculated inverse
    inv
}