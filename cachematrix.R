## makeCacheMatrix function create an object(matrix) m to store inverse 
## of input matrix(x), and list of 4 functions to set/get of m,
## and set/get cached m

makeCacheMatrix <- function(x = matrix()) {
    message("reset m")
    m <- NULL
    set <- function(y) {
        message("set parent x and m with input matrix")
        x <<- y
        m <<- NULL
    }
    get <- function() {
        message("get x from parent environment")
        x
    }
    setinv <- function(solve) {
        message("inverse matrix and save it to m in parent environemnt")
        m <<- solve
    }
    getinv <- function() {
        message("get m that is already computed from parent environment, will be NULL on first time")
        m
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## function cacheSolve using return of makecacheMatrix as argument
## to get cached result of matrix inverse or compute new inverse if
## cache is not found

cacheSolve <- function(x, ...) {
    m<-x$getinv()
    if(!is.null(m)) { 
        message("getting cached data")
        return(m)
    }
    message("cache not found")
    data<-x$get()
    m<-solve(data,...)
    x$setinv(m)
    m
}
