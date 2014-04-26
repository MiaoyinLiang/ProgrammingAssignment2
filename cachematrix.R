## By Miaoyin Liang

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	d <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
		d <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	isInvertible <- function() { ##determine if the matrix is invertible
		if(nrow(x)!=ncol(x)) ## the matrix is not invertible if it is not square
		{
			return(FALSE)
		}
		if(isSingular())## the matrix is not invertible if it is singular

		{
			return(FALSE)
		}
		return(TRUE)
		
	}
	isSingular <- function() { ##determine if the matrix is singular
		if(!is.null(d)) ##cache the determinant
		{
			if(d == 0)
			{
				return(TRUE)
			}
			else
			{
				return(FALSE)
			}
		}
		d <<- det(x) ##Calculate the Determinant of a Matrix

		if(abs(d) <= 1e-10)## if the determinant is closed to 0, then this matrix is singular
		{
			return(TRUE)
		}
		else
		{
			return(FALSE)
		}
	}
	list(set = set, get = get, 
	     setInverse = setInverse, 
	     getInverse = getInverse,
	     isInvertible = isInvertible, isSingular = isSingular)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
	if(!is.null(i)) {
		message("getting cached inverse")
		return(i)
	}
	if(x$isInvertible()){ ##if the matrix is invertible, then compute the inverse
		matrixX = x$get()
		i <- solve(matrixX)
		x$setInverse(i)
		return(i)
	}
	else{
		message("Matrix is not invertible")
		return(NULL)
	}	
}

