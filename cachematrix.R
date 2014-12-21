## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function create imatrix object which contains inverse of matrix x. 
#This function returns list of function to set and get matrix as well as to fetch 
#inverse of matrix value.

makeCacheMatrix <- function(x = matrix()) {

        imatrix <- NULL
        setmatrix <- function(m_in)
        {
                x <<- m_in 
                imatrix <<- NULL
        }
        getmatrix <- function() x
        
        setinverse <- function(inverse_in)
        {
                imatrix <<- inverse_in
        }
        
        getinverse <-function()
        {
                imatrix          
        }
        
        list(set = setmatrix, get = getmatrix,
                setinv = setinverse, getinv = getinverse)
}


## This function is to fetch inverse of matrix from cache. If origianl matrix
## has changed then inverse of matrix is recomputed and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        data <- x$get()
        imatrix <- x$getinv()
        imatrixnew <- solve(data)
        
        #check for ull and change in matrix
        if(!is.null(imatrix))
        {
                #check for data change. Invest of new and old should be different.
                l <- (imatrixnew == imatrix)   
                if( !any(l == FALSE))
                {
                        message("getting cached inverse matrix")
                        return (imatrix)
                }
        }
        
        ##recompute inverse of matrix
        #imatrix <- solve(data)
        x$setinv(imatrixnew)
        imatrixnew
                
}
