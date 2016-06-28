## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix<- NULL
    set <- function(y) { #set the value of the matrix
        x <<- y
        inverseMatrix<- NULL
    }
    get <- function() x #get the value of the matrix
    setInverse <- function(inverse) inverseMatrix <<- inverse #set the value of the Inverse Matrix
    getInverse <- function() inverseMatrix #get the value of the Inverse Matrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
    
    
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse() 
    if(!is.null(inverseMatrix)) { #checks to see if the inverse has already been calculated
        message("getting cached inverse Matrix data") #f so, get the Inverse from the cache and skip the computation
        return(inverseMatrix) #returned the cached Inverse
    }
    data <- x$get() #else , calculate the Inverse of the x
    inverseMatrix <- solve(data, ...) # b is taken to be an identity matrix I
    x$setInverse(inverseMatrix) # set the value of Inverted Matrix in cache
    inverseMatrix # return the calculated inverse of the matrix
}
