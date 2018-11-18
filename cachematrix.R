## The functions here are used to turn a given matrix into a cache matrix object.
## This object allows us to cache the inverse of the gievn matrix and pull it up later as needed.

## Creates an object that stores amatrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL;
    set <- function(y) {
        x <<- y;
        m <<- NULL;
    }
    get <- function() x;
    setinverse <- function(inverse) m <<- inverse;
    getinverse <- function() m;
    set(x);
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse);
}


## Gets the inverse of the given cache matrix object. 
## If the inverse hasn't been found yet, make the inverse, otherwise pull out the cached version.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data");
        return(m);
    }
    data <- x$get();
    m <- solve(data);
    x$setinverse(m);
    m;
}
