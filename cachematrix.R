## The following two functions calculate the inverse of a matrix and caches the value so it can be easily pulled again. 
## Example:
## x <- matrix(rnomr(4), 2, 2)      Create a matrix called 'x'
## cache_x <- makeCacheMatrix(x)    Create a special matrix
## cacheSolve(cache_x)              Calculates and returns the inverse of 'x'
## cacheSolve(cache_x)              Returns the inverse of 'x' with a cache message since it has been stored using the previous step


## This function creates a special matrix or "container" that outlines a list of functions that will store and retrieve the 
## matrix as well as the inverse matrix calculation. 



makeCacheMatrix <- function(x = matrix()) { ## make sure the given set of values will be a matrix
       
        ## cached inverse matrix will be stored in "value" 
        value <- NULL             
        
        ## Set value for matrix
        set <- function(y) {
            x <<- y
            value <<- NULL
        }
        
        ## Get value for matrix
        get <- function() x
        
        ## set value for inverse
        setinverse <- function(inverse) value <<- inverse
       
        ## get value for inverse
        getinverse <- function() value
       
        # return matrix with new functions
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function calculates the inverse of the matrix from the makeCacheMatrix 
## and caches it to the makeCachematrix function.
## It stores the calculated inverse matrix value to the "container" function above. 
## In this function, "x" should be the variable or name that we assigned the makeCacheMatrix function to. 

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
      
      ## Assign the variable "value2" as 
      value2 <- x$getinverse() 
      
      ## If value2 returns not null, the inverse is already calculated, so print with a message
      if(!is.null(value2)) {  
            message("getting cached data") 
            return(value2)
       }
      
      ## If "value2" is null, then the following computations happen: 
      data <- x$get() ## This grabs the matrix that was saved in the makeCacheMatrix function
      value2 <- solve(data, ...) ## calculates the inverse of the matrix using the solve function
      
      ## caches or stores the calculation  
      x$setinverse(value2) 
      
      ## print 
      value2
      
}
