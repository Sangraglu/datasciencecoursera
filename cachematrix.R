makeCacheMatrix <- function(x = matrix) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x #gets the value of the matrix
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m #gets the value of the inverse matrix
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

cacheSolve <- function(x, ...) {
  m <- x$getInv() 
  if(!is.null(m)) {
    message('getting cached data')
    return(m)
  }
  
  data <- x$get() #gets the value of the matrix
  m <- solve(data, ...) #calculates the inverse
  x$setInv(m) 
  m 
}