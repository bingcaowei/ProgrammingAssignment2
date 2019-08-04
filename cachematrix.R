## bingcaowei August 2019
## https://github.com/bingcaowei/ProgrammingAssignment2
## HW3 for R Programming Coursera

## Creates a special matrix object that can cache its inverse
## assume matrix is always invertible
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function (y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## computes the inverse of the special matrix returned by 
## makeCacheMatrix. If inverse has already been calculated 
## (and matrix has not changed), then cacheSolve should retrieve
## inverse from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        mat <- x$get()
        if(!is.null(inv)){
                message("getting cached data")
                return (inv)
        }
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
}
