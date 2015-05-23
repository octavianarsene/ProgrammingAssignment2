## Compute and cache the inverse of a square matrix 

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## cache variable
  invm <- NULL
  
  set <- function(y) {
    ## change the matrix
    x <<- y
    ## if the matrix is changed then
    ## set the inverse as NULL in order
    ## to trigger the new inverse matrix 
    ## computation
    invm <<- NULL
  }
  
  get <- function(){
    x
  } 
  
  ## set and get methods of the inverse matrix
  
  setinvmatrix <- function(invmNew){
    invm <<- invmNew
  }
  
  getinvmatrix <- function(){
    invm
  }
  
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)  
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated and the matrix has not changed, 
## then the inverse is returned from the cache otherwise it is computed and saved
## in the cache variable.

cacheSolve <- function(x, ...) {
  m <- x$getinvmatrix()
  ## check is the inverse is already cached or
  ## the matrix has been changed
  if(!is.null(m)) {
    message("Getting cached matrix inverse!")
    ## return the cached inverse of 'x'
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  ## set the new inverse matrix value
  x$setinvmatrix(m)
  m  
}