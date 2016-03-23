## I modified the example that caching the mean of a vector to caching inverse matrix of a matrix 'x'
## As Assignment instruction said, I assumed that the matrix supplied is always invertible,
##so I didn't make functions or processes to determining invertibility or non-invertible matrix

## This function calculate and cache inverse matrix of matrix 'x'.

## makeCacheMatrix makes a special "matrix" object that can cache its inverse.
## It is a list of 3 functions

makeCacheMatrix <- function(x = matrix()) {
		sv <- NULL
        #set <- function(y) {  #The function 'set'is defined in the example, but it is not used after. So I deleted this 'set' function, and they still work well
        #        x <<- y
        #        sv <<- NULL
        #}
        get <- function() x
        setsolve <- function(solve) sv <<- solve
        getsolve <- function() sv
        list(get = get,  #set = set,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
		sv <- x$getsolve()
        if(!is.null(sv)) {
                message("getting cached data")
                return(sv)
        }
        data <- x$get()
        sv <- solve(data, ...)
        x$setsolve(sv)
        sv
        ## Return a matrix that is the inverse of 'x'
}
