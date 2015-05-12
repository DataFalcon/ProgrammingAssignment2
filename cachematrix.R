## The pair of functions below cache the inverse of a matrix so that
## matrix inversion does not need to be repeated if the function
## cacheSolve is repeatedly called.  Instead, the already computed 
## inverse is returned  without repeating the inversion.

## Only square matrices have inverses and we are to assume that the
## the matrix supplied is always invertible

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## ***Important*** In the example, getting the mean of a vector, the mean
## and the marker indicating that the mean had previously been calculated were
## the same variable "m".  In the matrix case, it's easier just to have a
## seperate marker BeenSolved, a logical quantity that follows the solve function.
## That requires 2 new items in the list: getInverted (get the status of whether
## the matrix has been previously inverted or not) and setInverted (set this status)

makeCacheMatrix <- function(x = matrix()) {
    
    BeenSolved <- logical(1)
    m <- x
    set <- function(y) {
        x <<- y
        BeenSolved <<- FALSE
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    getInverted <- function() BeenSolved
    setInverted <- function() BeenSolved <<-TRUE
    list(getInverted = getInverted,
         setInverted = setInverted,
         set = set, 
         get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.  If the inverse has already been calculated (BeenSolved = True)
## AND the matrix has not changed (been re-initialized), then cacheSolve retrieves the inverse
## from the cache... a "lookup" instead of a potentially costly calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Calculate the value only if it doesn't exist already
        ## If it does exist (BeenSolved = True) return the previously calculated value
    m <- x$getSolve()
    BeenSolved <- x$getInverted()
    if(BeenSolved) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    x$setInverted()
    m
}

## Below is an example session with a 3x3 matrix "a" ("b" is the "special" matrix... a list)
## "c" is the first inversion of "b" ("a")
## "d" is the second inversion.  Notice that the d solution says "getting cached data"
## c and d are displayed and are identical as expected
## also the matrix product is displayed as [a %*% c].
## this product gives the identity matrix to a precision of 10^-16 as we would expect

## > a <- matrix(c(1,2,3,3,2,1,2,3,2), nrow = 3, ncol = 3)
## > a
##      [,1] [,2] [,3]
## [1,]    1    3    2
## [2,]    2    2    3
## [3,]    3    1    2
## > b <- makeCacheMatrix(a)
## > c <- cacheSolve(b)
## > d <- cacheSolve(b)
## getting cached data
## > c
##        [,1] [,2]   [,3]
## [1,]  0.125 -0.5  0.625
## [2,]  0.625 -0.5  0.125
## [3,] -0.500  1.0 -0.500
## > d
##        [,1] [,2]   [,3]
## [1,]  0.125 -0.5  0.625
## [2,]  0.625 -0.5  0.125
## [3,] -0.500  1.0 -0.500
## > c %*% a
##              [,1]          [,2]          [,3]
## [1,] 1.000000e+00 -1.110223e-16  0.000000e+00
## [2,] 5.551115e-17  1.000000e+00 -1.110223e-16
## [3,] 0.000000e+00  0.000000e+00  1.000000e+00
## > d %*% a
##              [,1]          [,2]          [,3]
## [1,] 1.000000e+00 -1.110223e-16  0.000000e+00
## [2,] 5.551115e-17  1.000000e+00 -1.110223e-16
## [3,] 0.000000e+00  0.000000e+00  1.000000e+00
## > 
