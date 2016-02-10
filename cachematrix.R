## Matrix inversion is a time-cost computation. My function
## can be used to create a matrix object and catch the inverse of this matrix, 
## avoiding to compute it repeatedly in various of computing processes.

## The first function, makeChacheMatrix creates a special "vector",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
# Assume that the matrix supplied is always invertible.

makeCacheMatrix<-function(x=matrix()){
  s<-NULL
  set<-function(y){
    x<<-y
    s<<-NULL
  }
  
  get<-function() x
  setsolve<-function(solve) s<<-solve
  getsolve<-function() s
  list(set=set,get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}

## The following function calculate the inverse of the
## special matrix created with the above funciton. However,
## it first check to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it caculates the inverse of the data and sets the value of
## the inverse in the cache via the setSolve function.

cacheSolve<-function(x,...){
    s<-x$getsolve()
    if(!is.null(s)){
      message("getting cached data")
      return(s)
    }
    
    data<-x$get()
    s<-solve(data,...)
    x$setsolve(s)
    s
}
