# Functions to create and use matrix conatiner objects
# that cache the inverse of that matrix.
# Computing the inverse is done lazily - so only
# if needed.


# Constructor for a matrix list object
# that caches the inverse of the contained matrix
makeCacheMatrix <- function(x = matrix()) {
    # variables in the closure (therefore the "c" prefix) of
    # the constructor
    cMatrix <- x
    cCachedInverse <- NULL
  
    # Sets a new matrix
    set <- function(m) {
        cMatrix <<- m
        # Clear the cache as this is most likely invalid now
        cCachedInverse <<- NULL
    }
  
    # Returns the conatined matrix
    get <- function() {
        cMatrix
    }
    
    # Returns the inverse of the contained matrix
    # with potentially additional parameters
    # for solve (in "...").
    # Computing the inverse is done lazily
    getInverse <- function (...) {
        if (is.null(cCachedInverse)) {
            # Compute the inverse as this was not done before
            cCachedInverse = solve(cMatrix, ...)
        }
        cCachedInverse
    }


  list(set = set,
       get = get,
       getInverse = getInverse,
       computeInverse = getInverse)
  
  # NOTE: No explict setInverse on purpose!
  # Having such a member function would allow
  # to set a matrix as inverse matrix which 
  # actually might not be the inverse of the
  # matrix in cMatrix - which would be confusing
  # and could lead to subtle bugs
  # (if cacheMatrix objects would be used in practice)
  # Instead the getInverse function is also 
  # provided under the name "computeInverse"
  # to allow users to express the intent that
  # the inverse should be computed now (if it hasn't already
  # happened)
}


# Computes the inverse of the matrix in x
# (if that hasn't happened already) and
# caches it
cacheSolve <- function(x, ...) {
    # Caching the inverse is already handled
    # in computeInverse respectively getInverse
    # (see above)
    x$computeInverse(...)
}


