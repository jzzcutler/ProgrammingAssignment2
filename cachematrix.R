## This package allows you to cache the inverse of a matrix rather than
## repeatedly computing it.  This can save a lot of time.  For instance,
## the following test run shows that caching the inverse can be 270 times
## faster than recomputing it 1000 times.
##     > TimeTest(1e4,1e4)
##     [1] "Time taken by solve:"
##        user  system elapsed
##       16.19    0.00   16.22
##     [1] "Time taken by cacheSolve:"
##        user  system elapsed
##        0.06    0.00    0.07
##

## This function creates a set of 4 functions that allow the inverse of a
## matrix to be cached so that it does not have to be recomputed each time it
## is needed.

makeCacheMatrix <- function(x = matrix()) {
    ## We are going to be defining functions within the lexical scope of this
    ## function.  Before we define any functions, we define a variable in this
    ## environment to hold (cache) the computed inverse.  This will make it
    ## available to all the functions we define, and it will be shared by them.
    cachedInverse <- NULL

    ## The "set" function allows us to assign a (new) value to the matrix
    ## whose inverse we want to cache.
    set <- function(y) {
        ##Assign a value to the x in the parent environment so that it will
        ##persist after this function ends.
        x <<- y

        ## We've changed the value of the data that the inverse was
        ## calculated on, so we have to set the value to NULL to indicate
        ## it must be computed the next time it is needed.
        cachedInverse <<- NULL
    }

    ## The "get" function returns the matrix whose inverse we want to cache
    ## (which is stored in our environment as "x").
    get <- function() {
        x
    }

    ## The "setInverse" function allows something external to this environment
    ## to compute the inverse of this matrix and then cache (store) it in this
    ## environment so it does not have to be recomputed.
    setInverse <- function(inverse) {
        cachedInverse <<- inverse
    }

    ## The "getInverse" function returns what the environment has saved as
    ## the cached value of the inverse of the cached matrix.  If the inverse
    ## has not yet been computed and cached (see the cacheSolve function),
    ## then we return NULL.
    getInverse <- function() {
        cachedInverse
    }

    ## Now we return a list containing the four functions we just created.
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )

}


## Return the inverse matrix of the stored matrix.  If it has not yet been
## computed, then compute it and cache it for future calls.  Otherwise,
## return the cached inverse.

cacheSolve <- function(x, ...) {
    ## We pull the cached value of the inverse of the stored matrix.
    inverse <- x$getInverse()

    ## The value returned by getInverse will be null if the inverse has not yet
    ## been stored.  Otherwise, we can return the cached value.
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    ## We have to actually compute the inverse because it wasn't in the cache
    ## so get the cached (stored) value of the matrix.
    data <- x$get()

    ## "solve" will return the inverse of our matrix.  The 3 dots allow the
    ## caller of cacheSolve to pass extra arguments to solve.
    inverse <- solve(data, ...)

    ## Now that we've computed the inverse, we use "setInverse" to store it
    ## so that the next time this function is called it will be available
    ## from the cache.
    x$setInverse(inverse)

    ## return the value of the inverse we just computed.
    inverse
}

## Run a timing test by creating a square matrix and then reporting the time
## taken when calling  cacheSolve "iterations" times to get the inverse of the
## matrix and reporting the time taken to call the solve function the same
## number of times.

TimeTest <- function(matrixSize, iterations) {
    ## Make sure the matrix is sqyare by taking the floor of the square root
    ## of the requested size and then squaring that to get the actual size.
    matrixRows = floor(sqrt(matrixSize))
    matrixSize = matrixRows * matrixRows

    ## Allocate the matrix
    m = matrix(rnorm(matrixSize),matrixRows, matrixRows)

    print("Time taken by solve:")
    pt = proc.time()
    for (i in 1:iterations) i <- solve(m)
    print(proc.time() - pt)

    print("Time taken by cacheSolve:")
    pt = proc.time()
    cm <- makeCacheMatrix(m)
    for (i in 1:iterations) i <- cacheSolve(cm)
    print(proc.time() - pt)
}
