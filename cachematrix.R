# This code is part of the assignment 2 of course R Programming
# Two functions are defined below which employ the caching
# mechanism to derive the inc=verse of the matric. If the inverse exists, then
# the value is obtained from the cache, else computed


# makeCacheMatrix is used to get the previous cached matrix, save the inverse of 
# the matrix, get the new matrix to validate with the previous matrix, get the 
# cached matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- matrix()
        old_mat <- matrix()
        get_new <- function() x
        get_inv <- function() inv_mat
        set_inv <- function(x) {
                        old_mat <<- x
                        inv_mat <<- solve(x)
                        inv_mat
        }
        get_old <- function() old_mat
        list(get_new = get_new, get_old = get_old, set_inv = set_inv, 
             get_inv = get_inv)
}


# cacheSolve function is used to get the matrix from the cache if the same matrix's 
# inverse was cache, otherwise computes the inverse of the matrix and caches the 
# result

cacheSolve <- function(x, ...) {
        old <- x$get_old()
        if(identical(old, x$get_new())){
                print('getting cached data!!')
                x$get_inv()
        }
        else{
                x$set_inv(x$get_new())
        }
}
