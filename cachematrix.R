## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Set the makeCacheMatrix. It creates a special "matrix"
##Cache inverses the input

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(a){
  x<<-a
  inv <<- NULL
}
get <- function()a
setInv <- function(inverse) inv<<- inverse
getInv <- function()inv
list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Write a short comment describing this function
## cacheSolve is a function that calculates the inverse of the special 'matrix'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getInv()
  if(!is.null(inv)){
    print("get cache result")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data,...)
  x$setInv(inv)
  inv
}
