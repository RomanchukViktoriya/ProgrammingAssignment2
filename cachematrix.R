## The two functions provided below are computing the inverse matrix for matrix x. 
## If the inverse has already been calculated, then there is no need to recalculate it and it will
## be returned, which saves computing time.

## The first function creates a special "matrix" object that can cache the input matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {

##we set a variable x_inv,in which we will store the final result later
    x_inv <- NULL

##we set the value of the matrix x
    setmatrix <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    getmatrix <- function() x

##we provide a function to assign computed before inverse matrix for x to x_inv, if exists
    setinverse<- function(inverse) x_inv <<-inverse

##we provide a function to obtain the cached inverse matrix for x
    getinverse <- function() x_inv

##we return list of functions as an R object
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}



## The second function actually calculates the inverse function. 

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
         x_inv <- x$getinverse()
         
## we check, if there was an inverse matrix computed before
    if (!is.null(x_inv)) {
    
## if was, we return it    
        message("getting inverse matrix")
        return(x_inv)
    } else {
 
## if not we calculate it for x and return it
        x_inv <- solve(x$getmatrix())
        x$setinverse(x_inv)
        return(x_inv)
    }
}
