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