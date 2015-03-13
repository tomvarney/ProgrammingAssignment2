## My 2 functions do the following:
#
##1. makeCacheMatrix creates a list of functions that allow a matrix to be set and retrieved (if it exists),
# and to have its inverse set and retrieved from the cache (if it exists)
#
## 2. cacheSolve can be used to return the inverse of the passed "special" matrix (whcih has been created
# using makeCacheMatrix).  If the inverse has already been calculated and stored in the persisted xinv 
# then it will return that one, else it will calculate the inveser  using "solve", store that in xinv, and 
# return that value as well.  Note that the particular "xinv" that is searched for is the one that is stored
# in the environment that the "special" matrix was defined in.

## Write a short comment describing this function
#This function creates a special matrix that has set/get functions as described above
#it creates a "special" matrix that includes functions for setting and getting its values and 
# the vlues for the inverse (and also for its defined-in environment)

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  print(environment())
  evn <- environment()
  print(parent.env(evn))
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) xinv <<- inverse
  getinverse <- function() xinv
  getevn<- function() environment()
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       getevn = getevn)
}


## Write a short comment describing this function
#This function allows user to pass a "special" matrix as defined above, and get the
#value of its inverse returned.
#This value will be retrieved from cache (in the special matrix's defined-in environment) if available.
# if the inverse is not already stored, it is calculated, stored in the defined environment, and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinverse()
    if(!is.null(xinv)) {
      message("getting cached data")
      return(xinv)
    }
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinverse(xinv)
    xinv
}
