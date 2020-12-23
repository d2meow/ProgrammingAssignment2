## Put comments here that give an overall description of what your
## functions do
## Two function are used to create a matrix and stores the inverse of the matrix.

## makeCacheMatrix creates a matrix object and can store its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        #set value of matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #get value of matrix
        get <- function() x
        
        #set inverse of matrix
        setinverse <- function(solve) inv <<- solve
        
        #get inverse of matrix
        getinverse <- function() inv
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## cacheSolve function calculates the inverse of the matrix returned by makeCacheMatrix and if the inverse already exist then return the stored inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.na(inv)) {
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setmean(inv)
        return(inv)
}
