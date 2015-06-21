## Two functions makeCacheMatrix and cacheSolve to solve for R Programming Assignment2
	## makeCacheMatrix creates a special "matrix" object and consists of list of functions- 
		##	setmatrix:	updates matrix to new set value
					switches flagchange indicator if the input matrix is same as the original matrix
		##	getmatrix:	retrieves matrix
		##	setinverse:	calculates inverse of input matrix
		##	getinverse: retrieves inverse matrix
	##cacheSolve calculates / retrieves inverse of specified matrix based on flagchange indicator

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	flagchange <<- -1						##flagchange default value is -1
      setmatrix <- function(y = matrix()) {
                if(identical(x,y)=="TRUE") {
				flagchange <<- 0			##flagchange set to 0 if array is unchanged
			}
		    else {
				flagchange <<- 1			##flagchange set to 1 if array is different
				x <<- y
				matrixinv <- matrix(NA,nrow(x),ncol(x)) 
			}
        }
      getmatrix <- function() x
	##computes matrix inverse
      setinverse <- function(y = matrix()) matrixinv <<- solve(y)	
      getinverse <- function() matrixinv
      
	list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates / retrieves inverse of specified matrix
## Flagchange vector helps identify if the matrix is a repeat (0) or new assignment
## The inverse is retrieved from cache if the matrix is unchanged else calculated

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ## Inverse matrix computation for original input matrix
	if (flagchange == -1) {
		##compute inverse from setinverse function 
		data <- x$getmatrix()
		flagchange <<- 0
		matrixinv <- x$setinverse(data)
		return(matrixinv)
	}
	## Inverse matrix computation for updated input matrix
	else if(flagchange == 1) {
		message("Computing inverse of updated matrix...")
		data <- x$getmatrix()
		flagchange <<- 0
		matrixinv <- x$setinverse(data)
		return(matrixinv)
        }
	##Get inverse that has been stored in cache as input data is unchanged
	message("Getting cached data...")
	matrixinv <- x$getinverse()
	matrixinv
}
