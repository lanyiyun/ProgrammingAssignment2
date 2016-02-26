#' function to create a matrix to be inversed 
#' 1. set the value of the matrix
#' 2. get the value of the matrix
#' 3. set the value of the inversed matrix
#' 4. get the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
        InvsMtx <- NULL
        set <- function(y) {
                x <<- y
                InvsMtx <<- NULL
        }
        get <- function() x
        setInvsMtx <- function(temp) InvsMtx <<- temp
        getInvsMtx <- function() InvsMtx
        list(set = set, get = get,
                setInvsMtx = setInvsMtx,
             getInvsMtx = getInvsMtx
        )
        
}

#'function to calculate the inverse matrix created by the makeCacheMatrix function
#'will first check to see if there is a cached value. If so, it gets the value out of
#'the cache and skips the computation, otherwise, it will calcucalte the inverse matrix
#'and return the result
cacheSolve <- function(x, ...) {
        InvsMtx <- x$getInvsMtx()
        if(!is.null(InvsMtx)) {
                message("getting cached data")
                return(InvsMtx)
        }
        data <- x$get()
        InvsMtx <- solve(data, ...)
        x$setInvsMtx(InvsMtx)
        InvsMtx
}