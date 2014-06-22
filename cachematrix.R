## These functions are mainly for computing an inverse of a matrix effectively by caching.

## makeCacheMatrix - creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    mat<-NULL
    set <- function(y){
        x<<-y
        mat<<-NULL
    }
    get<-function() x
    setinverse <- function(solve) mat<<-solve
    getinverse<-function() mat
    list(set=set,get=get,
         setinverse = setinverse,
         getinverse = getinverse)
    

}


## cacheSolve - computes the inverse of the special matrix from the makeCacheMatrix function. 
##              If the inverse has already been calculated, then the cachesolve returns the message, 
##              "getting cached data", and the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mat<-x$getinverse()
    if(!is.null(mat)){
        message("getting cached data")
        return(mat)
    }
    data<-x$get()
    mat<-solve(data, ...)
    x$setinverse(mat)
    mat

}
