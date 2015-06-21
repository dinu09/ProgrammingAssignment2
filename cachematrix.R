makeCacheMatrix <- function(x = matrix()) 
{
        inv = NULL
        set = function(y)
        {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}
cacheSolve <- function(x, ...)
{
        inv = x$getinv()
        # if the inverse has already been calculated
        if (!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        }
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}
