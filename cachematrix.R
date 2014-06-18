## set: inputs data (matrix) for future computations and resets the computed inverse matrix (if the data has changed)
## get: returns data that is currently in memory (has been 'set')
## setinv: stores the computed inverse matrix
## getinv: returns the computed inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <<- NULL
    set <- function(y){                 
        last <- get()                   ##fetch previous matrix
        if (identical(last, y)) {       ##check if new matrix is identical to previous one
            message("identical to last matrix..")        
        }else{
            x <<- y                     ##assign the new matrix to x
            inv <<- NULL                ##delete last solution so that a new one can be computed
        }
    }
    get <- function() x                 ##return matrix
    setinv <- function(inverse) inv <<- inverse     ## 
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Fetches solution if it has been computed for the current matrix
## Otherwise the program fetches data, calculates the inverse matrix, saves it and returns it
cacheSolve <- function(x, ...) {
    inv <- x$getinv()                   ##fetch current solution
    if(!is.null(inv)) {                 #if there is already a sloution, return it along with a message
        message("loading cached solution...")
        return(inv)
    }
    data <- x$get()                     ##fetch matrix to be inverted
    inv <- solve(data)                  ##compute inverted matrix
    x$setinv(inv)                       
    inv
}
