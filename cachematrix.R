## makeCache matrix provides a list of functions 
## namely- set,get,setinverse and getinverse.
## MakeCache Matrix goes hand in hand with cachesolve

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL  
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<- function(inverse) m<<-inverse
  getinverse<- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Cachesolve checks whether inverse has been computed
##If yes, cache value is returned from previous computation
##If no, the function computes the inverse 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
