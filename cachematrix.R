## makeCacheMatrix creates a new matrix object, assigns rm<-null value to it (set function)
## get functon gets the existing matrix
## setinverse function sets a rm value, received from cacheSolve function if rm was not present before
## getinverse function gets a rm cached value
## list contains the list of internal functions, "rules" of how they are accessed by a calling function


makeCacheMatrix <- function(x = matrix()) {
        rm <- NULL 
        set <- function(y) {
                x <<- y
                rm <<- NULL
        }
        get <- function() {x}
        setinverse <- function(solve) {rm <<- solve}
        getinverse <- function() {rm} 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve function calculates the inverse matrix
## first it checks rm value
## if rm value is not null, it throws a message, gets caching data, returns rm
## if rm value is null, 
## it fetches the matrix (data variable)
##  it calculates the inverse values
## it "sends" the value to the makeCacheMatrix via x$setinverse,
## it returns the inverse values of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        rm <- x$getinvers()
        if(!is.null(rm)) {
                message("getting cached data")
                return(rm)
        }
        data <- x$get()
        rm <- solve(data, ...)
        x$setinverse(rm)
        rm
}
