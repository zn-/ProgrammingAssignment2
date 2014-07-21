## Functions that cache the inverse of a matrix


## Function to create a special "matrix" object, which returns a list
## containing functions to set, get value of matrix and set, get value 
## of inverse

makeCacheMatrix <- function(x = matrix()) {
      Inv <- NULL
      set <- function(y){     ##method to set value of matrix
        x <<- y
        Inv <<- NULL
      }
      get <- function() x     ##method to get value of matrix
      set_inverse <- function(inverse){
        Inv <<-inverse        ##method to set inverse of matrix
      }
      get_inverse <- function() Inv ##method to get inverse of matrix
      list(set=set, get=get,        ##returning the list containing all methods
           set_inverse=set_inverse, get_inverse=get_inverse)

}


## Funtion to calculate the inverse of special "matrix" created above
## if the inverse of matrix is already calculated, then source it from 
## the mean and skip the computaion

cacheSolve <- function(x, ...) {
        
      I <- x$get_inverse()   ## fetching the inverse of 'x'
      if(!is.null(I)){
        message("getting cached data")
        return(I)            ## Return inverse of 'x' from the cache
      }
      data <- x$get()       ## fetching the matrix 'x'
      I <- solve(data, ...) ## calculating the inverse
      x$set_inverse(I)      ## setting the calculated inverse in cache
      I                     ## returning the inverse  
}
