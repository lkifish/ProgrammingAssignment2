## The 2 functions are used to create a special object
## theat stores a matrix and cache's its mean.

## the "makeCacheMatrix" function creates a spcial
##vector,containing a function to 
##1 set the value of the matrix
##2 get the value of the matrix
##3 set the value of the inverse matrix
##4 get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)m<<-inverse
  getinverse<-function()m
  list(set=set,get=get,setinverse=setinverse,
       getinverse=getinverse)
}

## the cacheSolve function calculates the inverse of
##the matrix,stored by the sepcial "vector" created with the above function.

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
