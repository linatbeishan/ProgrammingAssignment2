## Just like the provided vector example, I "imitate" to write the functions for matrix,
##  Below are the two functions that are used to create a special
## "matrix" object, actually it's a list, to store matrix and cache its inverted matrix

## makeCacheMatrix function: returns a special "matrix", containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix

## Please be noted that,
## 1. The argument of this function should be inputted a square invertible matrix,
## otherwise it will return error after the matrix is used in the second function
## 2. It may exceed your memory limit if you provide a large matrix,and get the error message
## like, "Error in matrix: too many elements specified", 
## in this case, please input a smaller matrix
## 3. Afeter you load the 2 functions, you can use the follow test example to feel the speed difference after using the "cache" feature 
## x <- 1:4000
## a <- makeCacheMatrix(diag(x))
## system.time(f <- cacheSolve(a))
## system.time(g <- cacheSolve(a))

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y)
        {
              x <<- y
              i <<- NULL
        }
        
        get <- function() x
        setInvert <- function(invt)  i <<- invt
        getInvert <- function() i
        list(set = set, get = get,
             setInvert = setInvert,
             getInvert = getInvert)
}


## caceSolve function: returns the inverted matrix of the matrix you provided,
## the function will first try to get the inverted matrix from the cached,
## if succeed to get(not a null object returned), it will return the iverted matrix,
## otherwise, it will try to solve the matrix, store the result in the cache, and return the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invt <- x$getInvert()
        if(!is.null(invt))
        {
                message("getting inverted matrix from cached")
                return(invt)
        }
        data <- x$get()
        invt <- solve(data, ...)
        x$setInvert(invt)
        invt
}
