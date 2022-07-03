## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Set the makeCacheMatrix. It creates a special "matrix"
##Cache inverses the input

makeCacheMatrix <- function(x = matrix()) ## sources the matrix to ve calculated
  {
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

cacheSolve <- function(x, ...) ## sources cache data
  {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getInv()
  if(!is.null(inv)){      ##check if the inverse is null
    print("print the cache result")
    return(inv)          ##returns the inverse value
  }
  data <- x$get()
  inv<- solve(data,...)    ##solves inverse value
  x$setInv(inv)
  inv                      ##gets the inverse of x (x$getInv)
}
