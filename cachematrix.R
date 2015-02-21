
## This function creates a "special" matrix object that:
## 1 - can cache its reverse
## 2 - expose a list of methods to access its data 

makeCacheMatrix <- function(x = matrix()) {
    # create a property "inverse" for cache storage
    invCache <- NULL
    
    # set the matrix and reset cache
    set <- function(y) {
        x <<- y
        invCache <<- NULL
    }
    # return the chached matrix
    
    get <- function() x
    
    # calculate reverse and set cache 
    setInverse <- function(inv) {
        invCache <<- inv
    }
    
    #return reverse matrix
    getInverse <- function() invCache
    
    #list of makeCacheMatrix exposed methods 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}



## The following function calculates the reverse of a given matrix.
## It first checks to see if the reverse has already been calculated. 
## If so, it gets the value from the cache and skips the computation. 
## Otherwise, it calculates the reverse of the given matrix 
## and sets the result in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # HINT: For java users, this is a kind of singleton
    
    # try to get the inverse matrix
    inverseMatrix <- x$getInverse()
    
    # if there's a value stored in cache...
    if(!is.null(inverseMatrix)) {
        
        #...return the cached value
        message("getting cached data")
        inverseMatrix
    }else{
        # ...otherwise 
        message("calculate data, store in cache, return reverse matrix")
        data <- x$get()
        #...calculate the reverse matrix
        inverseMatrix <- solve(data, ...)
        #...put the reuslt in cache 
        x$setInverse(inverseMatrix)
        #...and return the value 
        inverseMatrix    
    }
    
}

#  eg:
# >  myMatrix1 <- makeCacheMatrix(matrix(1:4,2,2))
# >  cacheSolve(myMatrix1)
#  calculate data, store in cache, return reverse matrix
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# >  cacheSolve(myMatrix1)
#  getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5