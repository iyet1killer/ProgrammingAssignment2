## The combination of two functions below provides a way
## to avoid repeatly calculating matrix inversion by caching
## existing result.

## make_cache_matrix creates a R object which can store a matrix
## and its inversion

make_cache_matrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      set_inverse <- function(solve) s <<- solve
      get_inverse <- function() s
      list(set = set, get = get,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}


## cache_solve checks if make_cache_matrix has a calculated matrix 
## inversion, if not, it will calculate a new one and return it back
## to make_cache_matrix.

cache_solve <- function(x, ...) {
      s <- x$get_inverse()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$set_inverse(s)
      s
}
