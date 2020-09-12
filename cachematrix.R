## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { # set the function to require a matrix as input
        m <- NULL #set object class as NULL
        set <- function(y){ 
                x <<- y #set a value y to the matrix
                m <<- NULL
        }
        get <- function(){x} #get the value of the matrix
        setInverse <- function(inverse) {m <<- inverse} #calculates the inverse of the matrix
        getInverse <- function() {m} 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getInverse()#Return a matrix that is the inverse of 'x' and assign it to 'm'
        if(!is.null(m)) # checks if the inverse has already been calculated
                message("getting cached data") #if so, displays "getting cached data" message
        return(m) #and retrieves it from cache memory
        
        matrix <- x$get()   #if the inverse has not been calculated, it will be done by the following function
        m <- solve(matrix,...)
        x$setInverse(m)
        m
}
