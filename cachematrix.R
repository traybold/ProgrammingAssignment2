# Functions makeCacheMatrix() and cacheSolve() work together
# to store a matrix and its inverse, so that it must be
# solved only once.
#
# makeCacheMatrix() returns a list of accessor functions
# that store and retrieve a matrix and its inverse.
# cacheSolve() uses those functions to calculate and cache
# the matrix's inverse to avoid repeating the calculation.



# makeCacheMatrix() takes a matrix and returns a list of 4
# functions that update or retrieve the matrix or its
# inverse.  The functions act on 2 slots, one for the matrix
# and the other for its inverse.
#
# set() stores a value in the matrix slot and sets the
# inverse slot to NULL
#
# get() returns the matrix slot's current value.
#
# setInverse() stores a value in the inverse slot.
#
# getInverse() returns the inverse slot's current value,
# which is NULL until setInverse() stores a value there.

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(matNew) {
        mat <<- matNew
        inv <<- NULL
    }
    get <- function() {
        mat
    }
    setInverse <- function(invNew) {
        inv <<- invNew
    }
    getInverse <- function() {
        inv
    }
    list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


# cacheSolve() takes a container created by
# makeCacheMatrix() and returns the inverse of the matrix
# slot's current value.  If the container's inverse slot is
# NULL, then the inverse is calculated and stored in that
# slot.  Otherwise, the inverse slot's current value is
# returned.

cacheSolve <- function(matHolder, ...) {
    # Return a matrix that is the inverse of 'matHolder$mat'
    if (is.null(matHolder$getInverse())) {
        matHolder$setInverse(solve(matHolder$get(), ...))
    }
    else {
        message("getting cached data")
    }
    matHolder$getInverse()
}

