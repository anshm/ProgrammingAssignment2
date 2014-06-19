## MakeCacheMatrix is used to create a Cached matrix object ; cacheSolve is used to 
## obtain the inverse of the matrix of the Matrix within the  "Matrix Object"

## MakeCacheMatrix creates and returns a "Matrix Object" consisting of a list of four functions to 
## and get both the matrix and its inverse input parameter : an invertible Matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  setMatrix <- function(v = matrix() )
  {
    x <<- v
    inverse <<- NULL
  }
  
  
  getMatrix <- function() {
    
   x
    
  }
  
  setInverse <- function(i=matrix())
  {
    inverse <<- i
    
  }
  
  getInverse <- function()
{
    inverse
    
  }
    

  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)

}


## cacheSolve function is used to return the inverse of a matrix object created by MakeCacheMatrix 
## cacheSolve simply returns the cached inverse stored in x if the inverse has already been calculated

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
          
        i = x$getInverse()
        
        if(is.null(i))
        {
          i = solve(x$getMatrix())
        }
        
        else
        {
          i = x$getInverse()
        }
        
        i
  
}
