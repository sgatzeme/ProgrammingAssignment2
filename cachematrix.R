## The following two functions (makeCacheMatrix, cacheSolve) will be used 
## to efficiently compute the inversion of a squared matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## the functions has an empty matrix as default input parameter
        i <- matrix() ## the inverse variable is initialized as an empty matrix
  
        set <- function(y = matrix()) { ## function for declaring the value of the input parameter x (super-assignment)
                x <<- y ## x gets the value of y
          
                i <<- matrix() ## the inverse matrix is initialized with an empty matrix every time we are trying to cache a new matrix
        }
  
        get <- function() x ## function for getting the cached matrix
  
        setinverse <- function(s) i <<- s ## function for declaring the value of the inverse matrix (super assignment)
  
        getinverse <- function() i ## function for getting the cached inverse
  
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## sumarizing list of functions as return value  
  
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) { ## takes an arglist as input parameters, x = special vector
        
        i <- x$getinverse() ## i gets the value of the cached inverse matrix
        
        data <- x$get() ## data gets the value of the cached input matrix
        
        ## print(i) - for testing purposes
        
        ## print(data) - for testing purposes
          
        if (is.matrix(i) && is.matrix(data) && dim(i) == dim(data)) { ## if i and data are matrices and have the same length,
                                                                      ## there has already been an inverse calculation
            
                print("Matrix is already inverted - getting cached data") ## getting the cached inverse matrix
            
                return(i) ## return value is the cached inverse matrix
          
        }
                
        else { ## if either data or i is not a matrix, the inverse matrix has to be calculated first
                  
                print("setting inverse matrix")
                  
                if (nrow(data) == ncol(data)) { ## check, if input matrix is a squared matrix and therefore invertible
                    
                        i <- solve(data) ## calculate the inverse matrix of data
                    
                        x$setinverse(i) ## set the value of i in the makeCacheMatrix environment
                    
                        return(i) ## return the inversed matrix
                    
                }
                  
                else { ## if matrix is not invertible, you have to correct the input matrix
                    
                        return("Matrix must be squared to be invertible")
                    
                }
                  
        }
  
}

## output runs

source("cacheMatrix.R")

input1 <- matrix (c(1,2,3,4), 2, 2)

z <- makeCacheMatrix(input1)

cacheSolve(z) ## calculate inverse matrix

cacheSolve(z) ## get inverse matrix from cache

input2 <- matrix (c(1,2,3,4,5,6), 3, 2)

w <- makeCacheMatrix(input2)

cacheSolve(w) ## input matrix is not invertible
