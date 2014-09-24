## This function will create the special "matrix" object that can catch its inverse


makeCacheMatrix <- function(x = matrix()) {
    inversion <- NULL
    set <- function(y){
        x <<- y
        inversion <<- NULL
    }
    get <- function()x
    setinversion <- function(solve) inversion <<-solve
    getinversion <- function() inversion
    list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}



## This function computes the inverse of the special"matrix" returned by makeCacheMatrix above. If the inverse has been caculated, the cacheSolve should return inverse from the cache



cacheSolve <- function(x, ...) {
    inversion <- x$getinversion()
    if(!is.null(inversion)){
        message("getting cached data")
        return(inversion)
    }
    data <- x$get()
    inversion <- solve(data, ...)
    x$setinversion(inversion)
    inversion
}
