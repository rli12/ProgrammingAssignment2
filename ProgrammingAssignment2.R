makeCacheMatrix <- function(x = matrix()){
        #this function is created and then used to input into cacheSolve function
        #inverse is initated to NULL
        inv <- NULL
        set <- function(y){
                #both x and inv are assigned to an object different from current environment
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...){
        #this returns the inverse of the original matrix
        inv <- x$getinverse()
        
        #checks to see if the inverse is already calculated
        if(!is.null(inv)){
                #if calculated run this
                message("getting cached data")
                return(inv)
        }
        #calculates inverse if it is not already calculated
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}
