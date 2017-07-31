## makeCacheMatrix creates a special "matrix" object from x (a matrix itself)
## that can cache its inverse and contains a list containing a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

     i <- NULL
     set <- function(y){
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get, 
                    setinverse = setinverse,
                    getinverse = getinverse)
}


## cacheSolve is a function that computes the inverse of the
## special "matrix" returbed by makeCacheMatrix. If the inverse
## was already calculated, then the cacheSolve retrieves the value of the
## inverse stored in the caches


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     if(!is.null(i)){
          message ("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}
