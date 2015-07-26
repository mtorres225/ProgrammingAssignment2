## Function returns the inverse of a matrix (assumed invertible)
## keeping a copy of the result, so that if the function is called again
##with the same matrix, the function returns the inverse from cache
#
#

makeCacheMatrix <- function(x = matrix()) {
        #x assumed invertible
        inv <- NULL
        set <- function(y) {
                x <<- y         #assign value in other environ
                inv <<- NULL
        }
        #following list allows to input into cacheSolve
        #with the correct call, depending on the input matrix
        #(if previously called, solution is in cache)
        
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Function returningg inverse of matrix x, from calc or from cache

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        if (!is.null(inv)){     
                # if the inverse is in cache, do not calc again 
                message("from cache")
                return(inv)
        }
        #otherwise, calculate inverse anew
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
         x$setinv(inv)
        
        return(inv)
}
