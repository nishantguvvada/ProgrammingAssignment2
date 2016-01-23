## The below pair of functions will compute the inverse
## of a matrix

## makeCacheMatrix creates a matrix object that can cache it's inverse
  
  makeCacheMatrix <- function(x = matrix()) {

    i<-NULL
    set<-function(y){
      x<<-y
      i<<-NULL
    }
    get<-function()x
    setinverse<-function(inverse)i<<-inverse
    getinverse<-function()i
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}
  
  

## This function computes the inverse of the matrix returned by the above 
## function. It checks if the inverse has already been calculated. If so
## it retrieves from the cache otherwise computes and saves in cache.
  
  cacheSolve <- function(x, ...) {
      i<-x$getinverse()
      if(!is.null(i)){
        message("getting cached data")
        return(i) ## Return a matrix that is the inverse of 'x'
      }
      data<-x$get()
      i<-solve(data,...)
      x$setinverse(i)
      i
  }
## Running the function
> x = rbind(c(1, -1/5), c(-1/5, 1))
> m = makeCacheMatrix(x)
> m$get()
     [,1] [,2]
[1,]  1.0 -0.2
[2,] -0.2  1.0
> cacheSolve(m)
          [,1]      [,2]
[1,] 1.0416667 0.2083333
[2,] 0.2083333 1.0416667
> cacheSolve(m)
getting cached data
          [,1]      [,2]
[1,] 1.0416667 0.2083333
[2,] 0.2083333 1.0416667
