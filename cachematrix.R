## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL      # variable im to store inverse matrix
  set <- function(y) {   # set function to check if it's square matrix and set the matrix
      nc <- ncol(y)     # get number of columns
      nr <- nrow(y)     # get number of rows
      if(nc != nr) {    # check if number of column=number of rows for square matrix
        print("Cannot create inverse for non square matrix. Please create a square matrix")
        exit
      }
    x <<- y           # set the matrix
    im <<- NULL       # set the inverse matrix to NULL
  }
  get <- function() x    # get function returns original matrix
  setinverse <- function(z) im <<- z    # set inverse matrix to given value z
  getinverse <- function() im           # get inverse matrix
  list(set = set, get = get,            # list containing a function to set, get, setinverse, getinverse of matrix
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()    #calling getinverse function, return value stored in im
  if(!is.null(im)) {       # checking if im is NOT NULL then print the message and return inverse matrix
    message("getting cached data")
    return(im)
  }                      # if im is NULL then call get function to get the matrix
  data <- x$get()        # get function is called
  im <- solve(data, ...)   # compute inverse of matrix using solve function
  x$setinverse(im)        # set inverse matrix
  im                      # return inverse matrix
}
