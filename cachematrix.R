## Caching the Inverse of a Matrix

## makeCacheMatrix creates a special "matrix" object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<-y
            m<<- NULL
      }
      get <- function () x
      setmatrix <- function (solve) m<<- solve
      getmatrix <- function() m
      list(set=set, get=get, setmatrix=setmatrix,
           getmatrix = getmatrix)

}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
       m<-x$getmatrix()
       if(!is.null(m)){
             message("getting cached data")
             return (m)
       }
       data <- x$get()
       m<- solve(data, ...)
       x$setmatrix(m)
       m
}

