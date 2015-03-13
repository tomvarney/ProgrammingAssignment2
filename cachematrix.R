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
  xinv <- NULL   #initalize the variable to store teh inverse
  print(environment())  #return info about environment to user ... here's where inverse will be cached
  evn <- environment()  # store that environemt
  print(parent.env(evn))  # print the parent of the environment 
  set <- function(y) {   # function to set the value of x if (x can be created empty, or can be modified)
    x <<- y
    xinv <<- NULL  #set the inverse to NULL because this is new data
  }
  get <- function() x  #retrieve value of x
  setinverse <- function(inverse) xinv <<- inverse  #function that actually persists the inverse
                                                    # uses "<<-" to assign value in definig environment
  getinverse <- function() xinv   #retrieves inverse from persisted variable
  getevn<- function() environment()   #retrieves info about environment
  list(set = set, get = get,       #retruns back to calling command the info about the created functions
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
    xinv <- x$getinverse()              #looks for invverse in cached variable
    if(!is.null(xinv)) {                #if found, then it will return that cached value
      message("getting cached data")    #indicate that cached value was used
      return(xinv)                      #return inverse
    }
    data <- x$get()               #if not found in cache then get the data so we can solve here
    xinv <- solve(data, ...)      #solve for the inverse  
    x$setinverse(xinv)            #save the result into the cache for next time
    xinv                          #return the inverse to calling function
}
