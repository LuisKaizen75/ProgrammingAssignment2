## Put comments here that give an overall description of what your
## functions do

# Both functions are based on lexical scoping. After reading the post
# "demystifying makeVector()" by Leonard Greski. I can tell that these two
# functions work together. First, "makeCacheMatrix()" build a list object
# that contains the set(), get(), setinverse() and getinverse() functions 
# defined in the function. Then the "cachesolve()" function uses this list 
# info and looks if the inverse of the matrix was already calculated. If it is
# not, then the function calculates it and store it in the list object thanks to
# the functions that the list object contains and thanks to lexical scoping,
# using the functions nested in "makeCacheMatrix()" the value of the inverse can
# be stored in the "inv" variable.

## Write a short comment describing this function

# this function has 4 functions in it (set, get, setinverse and getinverse).
# this functions can get or set values to the x and inv variables using the
# "<<-" operator which permits to edit the value of these variables that are in 
# the environment outside these nested functions. Then the main functions
# returns a list object that contains the 4 functions created. Thanks to
# lexical scoping the values of "x" (the matrix) and "inv" (the inverse of x)
# are safe and only can be read or written by the 4 functions created.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x <<- y
                inv <<- NULL
        }
        get<-function() x
        setinverse<-function(inverse) inv<<-inverse
        getinverse<-function() inv
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

# This function calls the functions in the list object created and look if the
# inverse was already calculated, if that is true, then the function only reads
# the value of the inverse and return it without calculating it again. If the 
# inversed was not calculated before, then the function calculate it, store it
# in the list object and return it to the user.

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting cache data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
