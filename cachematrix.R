## The code is for caching the value of time consuming computations, such as inverse of matrix.
## The scoping rukes are used to store the value, so we can retrieve it later rather then recalculate.


## Summary on makeCacheMatrix

# This function creates "special" matrix object and stores a list of functions:
# 1) set (changes the object stored in the main function)
# 2) get (returns the object stored in the main function)
# 3) setsolve (doesn't calculate the inverse of matrix, but stores the value of the input in a variable s)
# 4) getsolve (doesn't calculate either, but returns the value of the variable into the main function)
# Function list() stores all 4 functions in the main function makeCacheMatrix to assign it to an object.


makeCacheMatrix <- function (x = matrix()){
        
        s <- NULL
        set <- function (y) {
                x <<-y
                s <<-NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list (set = set, get = get, setsolve = setsolve,
              getsolve = getsolve)
}


## cacheSolve Summary
# This function checks if the value of the variable s exists (ie stored previosuly), and is not NULL. 
# If it exists, it returns the message and the value s, that is supposed to be the inverse.
# If not, s calculates through solve (data, ...) the inverse of the matrix, 
# and x$setsolve(s) stores it in the object, assigned above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        s <- x$getsolve()
        if(!is.null(s)) {
                message ("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve (data, ...)
        x$setsolve(s)
        s
        
}

#This is the code to cache the inverse of the matrix.
