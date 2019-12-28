## Two functions makeCacheMatric and cacheSolve are used to demonstrate a caching technique in R
##which can be used to avoid repeated time consuming calculations

## This functon creates a matrix object whose value can be assigned and retrived and it caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function(){x}
        setinv<-function(ans){
                inv<<-ans
        }
        getinv<-function(){
                inv
        }
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## This functon evaluates the inverse of the matrix returned by makeCacheMatrix
## If the matrix is the same and the inverse has already been calculated then the cached answer is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)){
                message("inverse already cached")
                return(inv)
        }
        data<-x$get()
        inv<-sol(data,...)
        x$setinv(inv)
        inv
}
