## Functions to cache the inverse of a matix 

## Below function will create a special matrix object which can cache it's inverse. 

makeCacheMatrix <- function(x = matrix()) {
        
        ## Assigning initial values 
        i <- NULL 
        
        ## Set the matrix 
        set <- function(y) {
                x <<- y
                i <<- NULL 
        }
        
        ## Get the matrix 
        get <- function() x
        
        ##Set the inverse of the entered matrix
        setinverse <- function(inverse) i <<- inverse
       
        ## Get the inverse of the entered matrix 
         getinverse <- function() i
         
        ## Return all the values
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The below function checks for the cache and if it is there then it shows the result and skips the compututation part.
## If not, then it computes the inverse and store the the value in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
         i <- x$getinverse()
        if (!is.null(i)){
                message("Collecting cached data ")
                return(i)
        }
        m <- x$get()
        i <- solve(m,...)
        x$setinverse(i)
        i 
}


#Sample input and output:
#Input matrix 'a'

# a <- matrix(c(2, 1, 3, 5), 2, 2) 
# b <- makeCacheMatrix(a)
# b$get()

# [,1] [,2]                     ;(the output is the entered matrix) 
# [1,]    2    3
# [2,]    1    5

#   cacheSolve(b)               ;(inverse of the entred matrix)
# [,1]       [,2]
# [1,]  0.7142857 -0.4285714
# [2,] -0.1428571  0.2857143

