## Put comments here that give an overall description of what your
## functions do
##For example we have an incomig object like a very big square matrix, which comes into our loop and we need to Reverse it using solve(X) function "on the fly". We assume that somethimes the incomig matrices could be repeated so we don't need to Reverse it again to save some processor resourses an of course time. We just can take the last already calculated Reversed matrix and equals the expected result to it

## Write a short comment describing this function . 
## This is the mostly inner function which 
makeCacheMatrix <- function(x = matrix()) {
    res <- NULL
    set <- function(y) { #This function is to set matrix value
      x <<- y
      res <<- NULL
    }
    get <- function() x #Here we get the matrix value
    setreverse <- function(reverse) res <<- reverse #Here we set the resulting INverse matrix
    getreverse <- function() res # Get the result of Inversed matrix
    list(set = set, get = get, #This is the list of described functions, which this function "makeCacheMatrix gives us back.
         setreverse = setreverse,
         getreverse = getreverse)
}  


## Write a short comment describing this function
## This function takes the list of four functions with theirs values
cacheSolve <- function(x, ...) {
  res <- x$getreverse() #Inversed matrix is set to 'res' variable
  if(!is.null(res)) { #Here we check if there is something exists in "res" variable and we get it back
    message("getting cached data")
    return(res)
  }
  data <- x$get() #data in our incoming matrix
  res <- solve(data, ...) #function Solve() from base package
  x$setreverse(res)
  res
  ## Return a matrix that is the inverse of 'x'
}
