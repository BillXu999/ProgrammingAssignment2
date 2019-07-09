## makeCacheMatrix is used to cread a matrix, and the second function is used to slove inverse of the matrix created by the first one 

## to creat the matrix

makeCacheMatrix <- function(x = matrix()) {
  I<- NULL
  set<- function(y){
    x<<-y
    I<<-NULL
  }
  get<- function() x
  setInverseMatrix<-function(Inverse) I<<-Inverse
  getInverseMatrix<-function() I
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## to represent the inverse of the Matrix

cacheSolve <- function(x, ...) {
  I<- x$getInverseMatrix()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data<- x$get()
  I<-solve(data, ...)
  x$setInverseMatrix(I)
  I
}

