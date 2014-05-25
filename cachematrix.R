## Put comments here that give an overall description of what your
## functions do
##
## At the end of coding the following 2 functions, the line that best describes this assignment is: "In this
## Programming Assignment will take advantage of the scoping rules of the R language and how they can be manipulated 
## to preserve state inside of an R object." as taken from the course Assignment web-page.
## 
## Suppose we have a costly computation in our program that we need to execute more than once. It makes sense to cache the results
## of the computation the first time so that we can fetch the result the second time onwards. R's lexical scoping allows us to implement 
## this cache. 
##
## This is demonstrated with the use of matrix inversion which is a costly process. A sample run for these functions is as follows:
## 1. create a matrix
## > ma=rbind(c(2, -3/4), c(-3/4, 2))
##
## 2. prepare the 'cached matrix'
## > new_ma=makeCacheMatrix(ma)
##
## 3. call cacheSolve to find the inverse of the matrix
## > cacheSolve(new_ma)
##          [,1]      [,2]
## [1,] 0.5818182 0.2181818
## [2,] 0.2181818 0.5818182
## 
## 4. when cacheSolve is called again for 'ma', the inverse is retrieved from the cache
## > cacheSolve(new_ma)
## getting cached data
## [,1]      [,2]
## [1,] 0.5818182 0.2181818
## [2,] 0.2181818 0.5818182
##
## When makeCacheMatrix is called, the matrix ma is passed to it. It is stored in this function's environment. It returns a list with
## elements as functions get, set, getInverse and setInverse.
## When cacheSolve is executed for the first time, the 'cached matrix', which is output of 'makeCacheMatrix' is passed to it. 
## cacheSolve first calls getInverse, which returns NULL. Then it calls get(). This is the first interesting part. Because of lexical 
## scoping, the value of x is accessed and returned from the environment in which it was defined, which is the makeCacheMatrix environment.
## Matrix Inverse is calculated and stored in the environment of makeCacheMAtrix.
##
## After that, on subsequent calls of cacheSolve, when getInverse is called, because of lexical scoping, the matrix inverse value is 
## retrieved from the environment in which makeCacheMatrix is defined. 
##
## Thus the environment for function definition of makeCacheMatrix serves as place where the cache is implemented.
##

## Write a short comment describing this function
##
## This function's environment serves as the location for cache implemtation. When called, the matrix is stored in this function's 
## environment. The function returns a list of functions for storing and retriving from the cache (which is this function's environment,
## and accessible to other functions because of R's lexical scoping)
##

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## Write a short comment describing this function
##
## This function makes use of the 'cached matrix within the makeCacheMatrix function'. When called for the first time, the getInverse 
## function in the list returns NULL. Then the get function is called. This retrieves the original matrix from the 'cache' 
## which is the environment in which the makeCacheMatrix function is defined. The matrix inverse is calculated using 'solve'. The inverse 
## is then stored in the cache by calling the setInverse function.
##  
## When cacheSolve is called subsequently, the getInverse function retrives the matrix inverse from the cache, which can be seen because of
## the "getting cached data" message.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}
