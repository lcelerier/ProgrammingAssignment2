## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function allow to initiate an Object with getters and setters
# If the inital x matrix change, the cache var "imatrix" is emptied

makeCacheMatrix <- function(x = matrix()) {
        # Initialize empty inverse matrix
        imatrix <- NULL
        
        # Basic setter
        set <- function(y){
                x <<- y
                # Empty potential inverse matrix cache
                imatrix <<- NULL
        }
        
        # Basic getter
        get <- function() x
        
        # Set inverse matrix in object
        setimatrix <- function(inverse) imatrix <<- inverse
        
        # Get inverse matrix from object
        getimatrix <- function() imatrix
        
        # Return getters and setters
        list(set=set, get=get, setimatrix=setimatrix, getimatrix=getimatrix)
}


## Write a short comment describing this function
# This function check for cache and if there is no cache, it solve the matrix and load object
# in any case, it send back an inverted matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # check to see if the matrix has already been solved
        imatrix <- x$getimatrix()
        if(!is.null(imatrix)){
                message("getting cached data")
                return(imatrix)
        }
        
        # if not, inverse the matrix and push result in object
        data <- x$get()
        inverse <- solve(data)
        x$setimatrix(inverse)
        inverse
}

# TESTS
myMatrix <- makeCacheMatrix(matrix( c(5, 1, 0,
                                      3, -1, 2,
                                      4, 0, -1), nrow=3, byrow=TRUE))
myMatrix$get()
cacheSolve(myMatrix)
cacheSolve(myMatrix)

myMatrix$set(matrix( c(4, 2, 2,
                       2, 3, 1,
                       2, 1, 3), nrow=3, byrow=TRUE))
myMatrix$get()
cacheSolve(myMatrix)
cacheSolve(myMatrix)

