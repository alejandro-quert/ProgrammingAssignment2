## You can use this two functions as follow:
## 1- Create an square matrix like: 
##    mtx = matrix(c(1,83,-111,15,37,-222,18,-46,333), nrow = 3, ncol = 3)
## 2- Invoke the first function using the created matrix and storing the result, like:
##    rl <- makeCacheMatrix(mtx)
## 3- Invoke the second function passing the result from the first one, like:
##    cacheSolve(rl)
## 4- Thats all!



## This function expects a matrix as a parameter.
## Even if this function works with any matrix, and will cache it, 
## please remember that a matrix inverse only exists for an square matrix.
## Also, if you are planning to calculate the inverse, 
## remember that the matrix cant be singular. 
## An example of a singular matrix:
## mtx = matrix(c(1,1,1,2,2,2,3,3,3), nrow = 3, ncol = 3)

makeCacheMatrix <- function(x = matrix()) {
	  rv <- NULL
        set <- function(y) {
                x <<- y
                rv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) rv <<- inverse
        getinverse <- function() rv
        list(set = set, get = get, setinverse= setinverse, getinverse = getinverse)
}


## If the matrix inverse is cached, it will be returned directly.
## If not, it will be calculated and returned.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
              message("getting cached data")
              return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setinverse(inverse)
      inverse
}
