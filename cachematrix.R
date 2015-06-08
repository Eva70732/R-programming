## These two functions should calculate and cache the inverve of a matrix.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        set <- function(x) {
                m <<- x
                inv <<- NULL
        }
        get <- function () return(m)
        setinv <- function(solve) return(inv) <<- solve
        getinv <- function() return(inv)
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This "cacheSolve' function returns the inverse of special matrix
## defined by "makecacheMatrix" above. But we have a special 
## condition here: if required inverse was calculated already 
## we should take it from the cache. 

cacheSolve <- function(m, ...) {
        inv <- m$getinv()
        if (!is.null(inv)) {
                message('getting cached data')
                return(inv)
        }
        data <- m$get()
        inv <- solve(data, ...)
        m$setinv(inv)
        return(inv)
}
