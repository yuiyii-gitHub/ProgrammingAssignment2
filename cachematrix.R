## Put comments here that give an overall description of what your
## functions do

## Set a matrix vector and its inverse values into cache

makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    set <- function(y) {
        x <<- y
        z <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) z <<- solve
    getSolve <- function() z
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Get inverse matrix in cache if available
## Otherwise, calculate the inverse

cacheSolve <- function(x, ...) {
    z <- x$getSolve()
    if(!is.null(z)) {
        message("getting cached data")
        return(z)
    }
    matrix <- x$get()
    z <- solve(matrix, ...)
    x$setSolve(z)
    z
}

#===============#
# TESTING STEPS #
#===============#
#---------------------------#
# 1. Create a matrix vector #
#---------------------------#
## x <- c(1,0,5,2,1,6,3,5,0)
## dim(x) <- c(3,3)
#------------------------------------------------------------#
# 2. Set the matrix vector into the makeCacheMatrix function #
#------------------------------------------------------------#
## cacheMatrix <- makeCacheMatrix(x)
## cacheMatrix$set(x)
## cacheMatrix$get()
#--------------------------------------#
# 3. Set the inverse matrix into cache #
#--------------------------------------#
## 3.1) Test in case the setSolve is NOT NULL
## cacheMatrix$setSolve(solve(x))
## cacheMatrix$getSolve()
## 3.2) Test in case the setSolve is NULL
## cacheMatrix$setSolve(NULL)
#--------------------------#
# 4. Call the cached value #
#--------------------------#
## cacheSolve(cacheMatrix)
#---------------------#
#   <<<<< END >>>>>   #
#---------------------#
