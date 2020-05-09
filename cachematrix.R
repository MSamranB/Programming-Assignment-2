## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inv<-NULL
  set<-function(y)#Initialize x and inverse variable in cache
  {
    x<<-y
    inv<<-NULL
  }
  get<-function() x #Retrieve x matrix from cache
  setinv<-function(inverse_mat) inv<<-inverse_mat #Store inverse in cache inv variable
  getinv<-function() inv #Retrieve inv matrix from cache
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv() #Retrieve inverse into inv variable
  
  #Check inverse is stored in cache previously to retrieve it in variable inv
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) #Calc Inverse on data
  x$setinv(inv)
  inv
  
}

#Test Input
c=rbind(c(1, -1/4), c(-1/4, 1))  
z<-makeCacheMatrix(c)
result<-cacheSolve(z)
result
