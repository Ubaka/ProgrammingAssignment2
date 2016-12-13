## Put comments here that give an overall description of what your functions do


## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## makecacheMatrix contains a list of four functions that do the following
## set() - sets the value of the matrix
## get() - retrieves the value of the stored matrix
## setinverse() - sets the value of the inverse of the matrix
## getinverse() - gets the value of the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
Inv_Mat <- NULL
set <- function(y) {
  x <<- y
  Inv_Mat <<- NULL
}
get <- function() x
setinverse <- function(inverse) Inv_Mat <<- inverse
getinverse <- function() Inv_Mat
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}



## This function calculates the inverse of the special "matrix" returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        Inv_Mat <- x$getinverse()
        if(!is.null(Inv_Mat)){
          message("getting cached data")
          return(Inv_Mat)
        }
        data <- x$get()
        Inv_Mat <- solve(data,...)
        x$setinverse(Inv_Mat)
        Inv_Mat
}

## Let us test this out in a matrix and its matrix inverse
m3 <- matrix(c(1/2,-1/4,-1,3/4),2,2)
e <- m3
e
f <- makeCacheMatrix(e)
## returns and caches the matrix inverse of m3
cacheSolve(f)


## Let us test this out in a matrix and its matrix inverse
m4 <- matrix(c(5/8,-1/8,-7/8,3/8),2,2)

h <- makeCacheMatrix(m4)

h$get()
##returns and caches the matrix inverse of m3
cacheSolve(h)



## Here we test out the matrix by multiplying by its cached inverse which gives us an invertible matrix which has a non-zero determinant
b <- matrix(c(1,0,5,2,1,6,3,4,0),3,3)
atmatrix <- makeCacheMatrix(b)
invmat <- cacheSolve(atmatrix)
cacheSolve(atmatrix)
round(b %*% invmat,1)                     