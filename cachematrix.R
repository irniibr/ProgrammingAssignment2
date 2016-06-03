## Cache the value of the inverse matrix

## Create special matrix, which is a list of function to
## 1. set value of the matrix
## 2. get value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## To calculate the inverse of the function makeCacheMatrix
## If there is a cache from above function, it will use the cache value rather than compute the number again

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
  ## Return the matrix which is the inverse of 'x'
}