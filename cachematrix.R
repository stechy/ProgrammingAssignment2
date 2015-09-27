## The two functions will store in a variable a special matrix object and also they
## will store, in the same R object, a cached version of a matrix inverse when this will be calculated
## with the second (helper) function

## This function will store in a variable a special matrix object and at the same time
## a cashed version of its inverse if it was calculated

makeCacheMatrix <- function (x = matrix()) {
    # at creation of a new matrix set the inverse to 'NULL'
    inverse_cached <- NULL
    ## creating the functions embedded for managing the matrix value and the cashed inverse matrix
    # set the new matrix object in a variable in the parent function
    set_matrix <- function (obj) {
        # the new matrix oject is set to the value of the given object
        x <<- obj
        # the cached inverse will be deleted (the new matrix object will have a different inverse)
        inverse_cached <<- NULL
        # both variables are assigned with "<<-" to be 'visibile' in the parent function scope
    }
    # get the value of an already created matrix object
    get_matrix <- function () {
        # the returning value is the matrix object itself
        x
    }
    # store (NOT calculating) the calculated inverse matrix value in a variable outside the local scope
    set_inverse <- function (inverse_value) {
        # store the given calculated inverse value in a variable in the parent function
        inverse_cached <<- inverse_value
    }
    # get the stored cached inverse value
    get_inverse <- function () {
        # the returning value is the cashed inverse value
        inverse_cached
    }
    # creating the list of the four function embedded
    list(set_matrix = set_matrix,
         get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function is an helper function for makeCacheMatrix(), for managing the cashed version
## of the inverse matrix - calculating it if it doesn't exist and/or returning it

cacheSolve <- function (x, ...) {
    # get the inverse value from makeCacheMatrix()
    inverse_cached <- x$get_inverse()
    # if the inverse cached value received is 'empty', then calculate the inverse for matrix and return it
    if (is.null(inverse_cached)) {
        # get the matrix data
        matrix_data <- x$get_matrix()
        # calculate the inverse
        inverse_cached <- solve(matrix_data)
        # set the calculated inverse value in the cached variable in the special matrix object
        x$set_inverse(inverse_cached)
        # return the inverse calculated (with message)
        message("Inverse calculated")
        inverse_cached
    } else {
        # otherwise, return the NOT empty cached value initially received (with message)
        message("Inverse get from cached value")
        inverse_cached
    }
}

