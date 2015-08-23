## makeCacheMatrix creates a special "matrix" object that can cache its inverse, which is
## really a list containing a function to 

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

## The implementation of this function is learned from Course Example, please check the 
## Markdown file: README.md

makeCacheMatrix <- function(x = matrix()) { 
## set the value 
invMatrix <- NULL 
set <- function(y) { 
 x <<- y 
 invMatrix  <<- NULL 
} 
## get the value 
get <- function() x 
    
## set the value of inverse of the matrix 
setinverse <- function(valOfInverse) invMatrix <<- valOfInverse 
getinverse <- function() invMatrix 
    
## get the value of inverse of the matrix 
list(set = set, get = get, 
    setinverse = setinverse, 
    getinverse = getinverse) 
 
  
} 


## cacheSolve returns a matrix that is the inverse of 'x'. 
## This function computes the inverse of the special “matrix” returned by makeCacheMatrix. 
## It checks whether the inverse has already been computed or not, If so, then cacheSolve should retrieve the inverse from the cache. 
## If not, it computes the inverse.


cacheSolve <- function(x, ...) { 
 
  
## get the inverse of the matrix         
invMatrix <- x$getinverse() 

 ## check if the inverse has already been calculated    
if(!is.null(invMatrix)) { 
   message("getting cached data!") 
     return(invMatrix) 
   } 
   ## if not, get the inverse of the matrix    
   data <- x$get() 
   invMatrix <- solve(data, ...) 
   ## set the inverse of the matrix  
   x$setinverse(invMatrix) 
   invMatrix 
 } 
