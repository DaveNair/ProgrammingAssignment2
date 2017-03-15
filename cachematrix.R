#2017-03-15 DaveNair
## Forgive me if anything in this is confusing.
## Although I based my script on the example provided,
## I have explained what I could in the comments
## I've left in a few of the original comments that came with this file

# so after much trial and error, this MUST be run as follows (replacing YourVar & YourMatrix with whatever):
#> YourVar <- makeCacheMatrix(YourMatrix)
#> cacheSolve(YourVar)


makeCacheMatrix <- function(x = matrix()) { #will need to CREATE a cached matrix to look into the cache!
    
    m <- NULL                           #cached inverse matrix is instantiated, but ==NULL
    set <- function(y) {
        x <<- y                         #when we call set(X), sets x & its solution (inv matrix)
        m <<- solve(y)           
    }
    get <- function() x                 #get() will essentially return x
    setinv <- function(inv) m <<- inv   #sets m to be the function's input
    getinv <- function() m              #again, getinv() will return m
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}



cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## this is an extremely misleading comment (from the original assignment)
    ## this function's job is to CHECK if we can use cached data
    ## and IF NOT, THEN SET, using functions (and solving), defined earlier
    
    ##I kept thinkingi I needed to do something like this:
    # x <- makeCacheMatrix(x)
    #due to the error I was getting. After all that, I figured that you need to run each func separately
    #i think its because i need to CREATE the cache, and THEN compare my input to it
    
    m <- x$getinv()             #get getinv() from x
    if (!is.null(m)) {              #if getinv() (cache) is NOT empty 
        message('Using cached data.')
        return(m)                   #then return cache
    } else {
        message('Found no cached data.')
    }
    #if cache WAS empty, function will continue (past return(m))
    data <- x$get()             #with no cache, "get()" x
    m <- solve(data, ...)       #now do the normal matrix inv
    x$setinv(m)                 #and set THIS to be the cache for next time
    #print(x$getinv())
    m #same as return(m)
}
