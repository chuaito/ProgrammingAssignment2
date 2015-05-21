## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
        mInv <- NULL
        
        set <- function(y) ## Set the value of the matrix
        {
               x <<- y
               mInv <<- NULL
               
        }
        
        get <- function() ## Get the value of the matrix
        {
                x
        }
        
        setInverse <- function(mInverse) ## Set the value of the inverse matrix a.k.a. mInv
        {
                mInv <<- mInverse
        }
        
        getInverse <- function() ## Get the value of the inverse matrix a.k.a. mInv
        {
                mInv
        }
        
        list(set = set, get = get,
             setInverse = setInverse, 
             getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## The following function calculates the mean of the special "vector" created 
## with the above function. However, it first checks to see if the mean has 
## already been calculated. If so, it gets the mean from the cache and skips 
## the computation. Otherwise, it calculates the mean of the data and sets 
## the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        mInv <- x$getInverse() ## Get mInverse form the outer enviroment
        
        if(!is.null(mInv)) ## If NULL returns the cached inverse matrix, and finishes the func
        {
                message("Inverse Cached!")
                return(mInv)
        }
        
        originalMatrix <- x$get() # Otherwise gets the matrix 
        mInv <- solve(originalMatrix,...) #  inverse the matrix
        x$setInverse(mInv) # cache the matrix
        
        mInv     # return the inverted matrix 
}
