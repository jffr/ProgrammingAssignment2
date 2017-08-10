## Below are two functions that are used to create a special object that
## stores a matrix and caches its inverse.


## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse =  getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse matrix has already been calculated. If so, it `get`s the inverse
## matrix from the cache and skips the computation. Otherwise, it calculates
## the inverse matrix of the data and sets the value of the inverse matrix 
## in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    ## If missing the second arguments, is taken to be an identity matrix 
    ## and solve will return the inverse of x.
    i <- solve(data)
    x$setinverse(i)
    i
}


# for example:
# z <- makeCacheMatrix(matrix(rnorm(400),20,20))
# cacheSolve(z)
