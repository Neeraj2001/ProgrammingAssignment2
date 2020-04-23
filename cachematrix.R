## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL

#set the value of the Matrix
  set_matrix <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }

  get_matrix <- function() x                              #get the value of the Matrix
  set_inverse <- function(inverse) inv_matrix <<- inverse  #set the value of the invertible matrix
  get_inverse <- function() inv_matrix                     #get the value of the invertible matrix
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse, get_inverse = get_inverse)

}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {

#get the value of the invertible matrix from the makeCacheMatrix function
          inv_matrix <- x$get_inverse()
        if(!is.null(inv_matrix)) {                       #if inverse matrix is not NULL
          message("Cached data")                       #Type message: data 
          return(inv_matrix)                             #return the invertible matrix
        }

#if value of the invertible matrix is NULL then  
        matrix_data <- x$get_matrix()                     #get the original Matrix Data 
        inv_matrix <- solve(matrix_data, ...)             #use solve function to inverse the matrix
        x$set_inverse(inv_matrix)                         #set the invertible matrix 
        return(inv_matrix)                               #return the invertible matrix
        ## Return a matrix that is the inverse of 'x'
}
