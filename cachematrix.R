## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The folowing function creates a list containg functions, that enable user 
# and other functions overwritng new data and extracting data (both the matrix
# and its inverse)

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL #sets the starting value of inverted matrix
        set <- function(y) {
                #function allowing to set the value of matrix x
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x #function allowing extracting the value of matrix x
        
        #allows storing inverse of matrix x computed by other function 
        #(cacheSolve in this case)
        setinv <- function(inverse) inv <<- inverse
        
        #allows extracting the value of the inverse of matrix x stored in cache
        getinv <- function() inv
        
        #compiles a list of all the functions above
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

#Function that computes the inverse of matrix stored in a list compiled with
#function makeCacheMatrix() and 'saves' it in this list or extracts the inverse 
#from the list (if possible)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                #if the value of the matrix inverse is different than null 
                #function assumes that it has already been calculated and 
                #extracts it from the list
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()     #'loads' matrix x from the list
        inv <- solve(data)  #calculates the inverse
        x$setinv(inv)       #"saves' it in the list
        inv                 #returns the value of the inverse
}
