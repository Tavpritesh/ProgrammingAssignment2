## This code comprises of two functions which together create a matrix and retrieve
## its inverse from cache if it has been already computed before.

#################################################################################
#                   1. Beginning of makeCacheMatrix                             #
# makeCacheMatrix creates a matrix with fields which will be used by cacheSolve #
#################################################################################

makeCacheMatrix <- function(input_mat = matrix()) {
    inv <- NULL;
    set <- function(y=matrix()){
        
#########################################################################        
#  Some extra code to check if the dimensions of the initialized matrix #
#           match with the dimensions being set                         #    
#########################################################################  
    if((ncol(y)==ncol(input_mat)) && (nrow(y)==nrow(input_mat)))
       {
           input_mat <<- y;
       }    
       else
       {
            stop("Input Dimensions don't match with dimensions of initialized matrix");
       }
      }
####################### Extra Code Ends Here ##################################
    
    get <- function()   input_mat
    setInverse <- function(inverse) inv <<- inverse 
    getInverse <- function()inv  
    list(set=set,get=get, getInverse=getInverse, setInverse=setInverse);
}

############################    End of makeCacheMatrix  #######################

###############################################################################
#                       2. Beginning of cacheSolve                            #
# cacheSolve fetches the inverse of a matrix if it has already been computed  #
# before, else it computes the inverse of the matrix and returns it           #
###############################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setInverse(inv)
    inv
}

############################    End of cacheSolve   ########################

