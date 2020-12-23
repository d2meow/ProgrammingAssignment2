## Two function, makeCacheMatrix and cacheSolve are used to create a matrix, calculate and store the inverse of the matrix in a special environment.

## makeCacheMatrix is a list of functions that set the value of the matrix, get the value of the matrix, set the value of the inverse and get the value of the inverse.
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
        #get inverse from list returned from makeCacheMatrix 
        inv <- x$getinverse()
        #if inv is not empty then return the inverse of x
        if(!is.na(inv)) {
                return(inv)
        }
        
        #get matrix x
        data <- x$get()
        #calculate the inverse of matrix x
        inv <- solve(data,...)
        #set calculated inverse
        x$setinverse(inv)
        return(inv)
}
