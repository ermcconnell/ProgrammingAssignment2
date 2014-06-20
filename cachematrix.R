## This code contains two functions. makeCacheMatrix should be called first
## followed by cacheSolve. These functions allow you to cache the calculation 
## of the inverse of a matrix. 

## makeCacheMatrix is a function that creates a list of four other functions to
## set a matrix, get the matrix, set the inverse of a matrix, and then get
## the inverse of that matrix if it has been set previously

makeCacheMatrix <- function(Mat = matrix()) {
    MatInv <- NULL
    set <- function(y) {
      Mat <<- y
      MatInv <<- NULL
    }
    get <- function() {
      Mat
    }
    setInv <- function(inverse) {
      MatInv <<- inverse
    }
    getInv <- function() {
      MatInv
    } 
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve is a function that takes the result of makeCacheMatrix and
## checks if the inverse of the matrix was already set. If it hasn't been set
## it computes the inverse. If it has been set, it finds the cached data and
## prints it. The function will tell you if it is supplying cached data.

cacheSolve <- function(Mat, ...) {
        ## Return a matrix that is the inverse of 'Mat'
  MatInv <- Mat$getInv()
  if(!is.null(MatInv)) {
    message("getting cached data")
    return(MatInv)
  }
  data <- Mat$get()
  MatInv <- solve(data, ...)
  Mat$setInv(MatInv)
  MatInv
}

## I used the following code to test my function:
## FirstMat <- matrix(c(2, 4, 3, 1), nrow=2, ncol=2)
## Test1<-makeCacheMatrix(FirstMat)
## cacheSolve(Test1)
## cacheSolve(Test1)
