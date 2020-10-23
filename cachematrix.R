## Caching the Inverse of a Matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}


#Solution 1
#B <- matrix(c(1,2,3,4),2,2)

#B1 <- makeCacheMatrix(B)
#cacheSolve(B1) 

#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5


#Solution 2
# mat<-matrix(c(1,7,9,4),2,2)
# mat1<-makeCacheMatrix(mat)
# cacheSolve(mat1)
#[,1]        [,2]
#[1,] -0.06779661  0.15254237
#[2,]  0.11864407 -0.01694915