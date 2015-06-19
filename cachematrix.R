##cache a matrix's inverse to avoid compute the inverse repeatedly

# this function creates a object which cache the matrix an it's inverse.
makeCacheMatrix <- function(x = matrix()) {
    # inverseMatrix will store the cached inverse matrix
    inverseMatrix <- NULL

    # Setter for the matrix
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    # Getter for the matrix
    get <- function() x

    # Setter for the inverse
    setInverse <- function(inverse) inverseMatrix <<- inverse
    # Getter for the inverse
    getInverse <- function() inverseMatrix

    # Return the matrix with our newly defined functions
    list(set = set, 
		get = get, 
		setInverse = setInverse, 
		getInverse = getInverse)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverse()

    # If the inverse is already calculated, return it
    if (!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }

    # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    inverseMatrix <- solve(data, ...)

    # Cache the inverse
    x$setInverse(inverseMatrix)

    # Return it
    inverseMatrix
}

# Example:
# > x <- matrix(rnorm(16), nrow = 4)          // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create our special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call the 2nd time, so return
#   
