'''
The function makeCacheMatrix generates a list of 4 functions that will serve 
as an input for the cacheSolve function. The 4 functions allow changing of the
matrix, returning the matrix, inverting the matrix and returning the inverse. 
A square invertable matrix is needed as an input for makeCacheMatrix.
'''

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL # intiate the inverse variable as NULL
        
        set <- function(y) {
                x <<- y # replace x with the new variable y
                inverse <<- NULL # set inverse to NULL - function 1
        }
        get <- function() x # retrieve x - function 2
        setinverse <- function(solve) inverse <<- solve # calculate inverse - function 3
        getinverse <- function() inverse # retrieve inverse - function 4
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) # list of the 4 functions
}


'''
The function cacheSolve uses the functions returned by makeCacheMatrix to
compute the inverse of the given matrix in makeCacheMatrix. If the inverse has
already been calculated and the matrix remained the same, then the function will
retrieve and return the result from the cache.
'''

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse() # get the inverse from makeCacheMatrix
        
        if(!is.null(inverse)) { # check if the inverse is not equal to zero
                message("getting cached data")
                return(inverse)
        }
        
        data <- x$get() # retrieve the matrix if inverse is equal to zero
        inverse <- solve(data, ...) # assign the inverse
        x$setinverse(inverse) # set the inverse in the cache
        inverse # return the inverse
}