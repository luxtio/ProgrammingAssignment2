## Put comments here that give an overall description of what your
## functions do

## How to use:
## let h a square matrix (then) e.g.:
##      h <- matrix (c(1,2,2,1), nrow=2, ncol=2) 
## let mat a makeCacheMatrix object from h
##      mat<-makeCacheMatrix(h)
## find the inverse matrix. The first time use solve function
##      cacheSolve(mat)
## find the inverse matrix. The second time use cache 
##      cacheSolve(mat)
##      getting cached data 

## Write a short comment describing this function
## Function to create an object in order to manage a square matrix (x) and the 
## structure to cache the inverse of matrix x (field invM)
makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setInvM <- function(newInvM) invM <<- newInvM
        getInvM <- function() invM
        list(set = set, get = get,
             setInvM = setInvM,
             getInvM = getInvM)
        
}


## Write a short comment describing this function
## Function that can calculate the inverse of a square matrix hold in a makeCacheMatrix object
## if the inverse is already cached then no solve operation is computed
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInvM()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        data <- x$get()
        invM <- solve(data)
        x$setInvM(invM)
        invM
}

