## This application takes a square matrix as input and calculates its invert
## If the invert has for the exact same input matrix has been already calculated, 
## it returns the invert from the cache instead of re-calculating it
## by using lexical scoping. 

## makeCacheMatrix function:
## The function takes the matrix set by the user. 
## It initialises the two objects 'x' and 'invert'
## It defines the 'setters' and 'getters' and 
## then returns them in a list of four objects. 


makeCacheMatrix <- function(x = matrix()) {

  invert <- NULL
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  get <- function() {x}
  setinvert <- function(inv) {invert <<- inv}
  getinvert <- function() {invert}
  list(
    set = set,
    get = get,
    setinvert = setinvert,
    getinvert = getinvert
  )
}

## cacheSolve function
## The if-statement checks whether he 'invert' object is NULL
## If it is NULL, it means it was reset by the makeCacheMatrix 
## function as a new matrix was entered, and it consequently 
## cacheSolve will calculate the invert. 
## If it is not NULL, it will return the cached invert. 

cacheSolve <- function(x, ...) {

  invert <- x$getinvert()
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setinvert(invert)
  invert

}
