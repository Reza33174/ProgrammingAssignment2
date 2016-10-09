## Caching the Inverse of a Matrix - Cashing the Inversed Matrix for Future Use



## First function (makeCacheMatrix) creates a  "matrix" object then it inverses the "matrix" and then cashe the "inversed matrix"

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)

}


## Second Function (cacheSolve) computes the inverse of a "matrix" returned by the First Function (makeCacheMatrix) 
## In the Second Function (cacheSolve), if the inverse of the matrix  has already been calculated (and the matrix has not changed), 
## then the Second Function (cacheSolve) retrieves the "inversed matrix," which is cashed by the First Function
## and prints the message "getting cached data".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
