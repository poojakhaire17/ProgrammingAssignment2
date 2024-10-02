# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the inverse cache
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # Function to get the inverse of the matrix
    getInverse <- function() inv
    
    # Return a list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Try to get the cached inverse
    
    # If the cached inverse exists, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, calculate the inverse
    data <- x$get()  # Get the matrix
    inv <- solve(data, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the inverse for future use
    
    inv  # Return the computed inverse
}


# Example usage
my_matrix <- makeCacheMatrix(matrix(c(2, 4, 3, 1), nrow=2, ncol=2))

# Calculate the inverse (this will compute and cache it)
inverse1 <- cacheSolve(my_matrix)
print(inverse1)

# Calculate the inverse again (this will use the cached value)
inverse2 <- cacheSolve(my_matrix)
print(inverse2)
