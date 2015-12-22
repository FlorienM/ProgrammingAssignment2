## This program returns the inverse of a matrix, 
## Either from the cache or it gets calculated.


## makeCacheMatrix creates a list with 4 objects

makeCacheMatrix <- function(x = matrix()) {           
        m <- NULL                                  #the cached result is initialized to NULL
        set <- function(y) {                       #set function,used when x is reinitilazed
                x <<- y
                m <<- NULL
        }
        get <- function() x                         #one line function that stores x
        setinverse <- function(solve) m <<- solve   #one line function that sets m to the inverse
        getinverse <- function() m                  #one line function that stores m
        list(set = set, get = get,                  #return a list of 4 objects
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of a square matrix

cacheSolve <- function(x, ...) {                    
        m <- x$getinverse()                         #retrieve the cached value   
        if(!is.null(m)) {                           #if cached value is not NULL
                message("getting cached data")
                return(m)                           #return cached value
        }
        data <- x$get()                             #get matrix
        m <- solve(data, ...)                       #compute inverse
        x$setinverse(m)                             #store inverse
        m                                           #return inverse matrix
}
