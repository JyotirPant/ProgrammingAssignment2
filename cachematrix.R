## This is the method for Computing the inverse of a square invertible matrix 

## makeCacheMatrix fuction intakes a matrix and stores its value and Inverse in the Cache.
##get() method can be used to retrieve the value of the matrix
##set() method can be used to set the value of new matrix
##getInverse() method can be used to retrieve the value of the inverse matrix
##setInverse() method can be used to set the new value of Inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
inv<- NULL
set<- function(y){
  x <<- y
  inv <<- NULL
}
get<- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set , get = get , setInverse = setInverse , getInverse = getInverse)
}


## This function returns the inverse of the matrix pprovided in the input
## first it is checked whether the matrix is a square matrix with determinant not 
## equals to zero. If any of these conditions fail, the inverse is set to be null.
## The vale of the inverse of a singular matrix is thus provided in the cache of the memory.

cacheSolve <- function(x, ...) {
  if(NCOL(x$get())==NROW(x$get()) && det(x$get())!=0){
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    else{
    data <- as.matrix(x$get())
    inv <- solve(data)
    x$setInverse(inv)
    return(inv)
    }  
  }
  else{
    message(paste("The matrix entered is a" , NROW(x)," X " , NCOL(x)," matrix. Please Enter an n X n matrix "))
    return(NULL)
     }
}
