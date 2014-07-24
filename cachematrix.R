## 1.makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.
##
## 2.cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.


## MakeCacheMatrix is used to define the functionsassociated with storing
## a matrix into a Cache (list) and making it accessible outside of this function


makeCacheMatrix <- function(x = matrix()) {
      #Initialise Storage
      store <- NULL

      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      ##Define the List of Functions
      
      get <- function() x 
      setinverse <- function(sinv) store <<- sinv #Make Available outside this function
      getinverse <- function() store
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x' 
## If the Cache of this matrix already exists then use that

cacheSolve <- function(x, ...) {


      store <- x$getinverse() #Check if 'Special' Matrix is already stored
      
      if(!is.null(store)) { #If 'store' is not empty, then return it
            message("found cached data, using that")
            return(store) #end here as returned the value of the store
      }
      
      ## If you are here then line 39 has returned a null value! 
      ## So looks like our processer needs to earn it's keep!
      
      message("No cached inverse of this matrix in the store") #Just to let us know its doing calcs!
      
      data <- x$get() # Put the value of the Special Matrix into 'data'
      store <- solve(data, ...) #execute the inverse of the matrix
      x$setinverse(store) # Now push the Solved values back into the Store
      
      store #Display the value of the inverse
}
