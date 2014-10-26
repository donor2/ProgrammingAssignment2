## Caching the Inverse of a Matrix


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( x = matrix() ) {

        x_inverse <- NULL
        
        setMatrix <- function( mtrx ) {
                x <<- mtrx
                x_inverse <<- NULL
        }

        getMatrix <- function() {
                x
        }

        setInverseMatrix <- function( solve  ) {
                x_inverse <<- solve 
        }

        getInverseMatrix <- function() {
                x_inverse
        }

        list( setMatrix = setMatrix,
              getMatrix = getMatrix,
              setInverseMatrix = setInverseMatrix,
              getInverseMatrix = getInverseMatrix
              )
        
}

## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix
cacheSolve <- function( x, ... ) {
        
        x_inverse <- x$getInverseMatrix()
        
        ##If the inverse has already been calculated
        ## then cacheSolve should retrieve the inverse from the cache
        if ( is.null( x_inverse ) ) {
                matrix <- x$getMatrix()
                x_inverse <- solve( matrix, ... )
                x$setInverseMatrix( x_inverse )
        }

        ## Return a matrix that is the inverse of 'x'
        x_inverse
}
