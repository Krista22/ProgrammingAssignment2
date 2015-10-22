# The functions below are a pair of functions that cache the inverse of a matrix.
# Creates a matrix that can cache its inverse.
makeCacheMatrix=function(x=matrix()){
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        setinverse<-function(solve) i<<-solve
        getinverse<-function()i
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

# Computes the inverse of the matrix returned by makeCacheMatrix.  If the inverse
# was previously calculated the inverse is retrieved from the Cache.
cacheSolve=function(x,...){
        i<-x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
        }

