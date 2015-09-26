## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){

	## Initialize the inverse property
	arq <- NULL

	## Method to set the matrix
	setM <- function(M){
		x <<- M
		arq <<- NULL
	}

	## Method the get the matrix
	getM <- function()
		x

	## Method to set the inverse of the matrix
	setInverse <- function(solve)
		arq <<- solve

	## Method to get the inverse of the matrix
	getInverse <- function()
		arq

	## Return a list of the methods
	list(
			setM = setM, 
			getM = getM, 
			setInverse = setInverse, 
			getInverse = getInverse
		)
}

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the 
## inverse has already been calculated (and the matrix has not changed), then the `cachesolve` should 
## retrieve the inverse from the cache

cacheSolve <- function(y, ...){

	## Get a matrix that is the inverse of 'y'
	inverse <- y$getInverse()

	## Just return the inverse if its already set
	if(!is.null(inverse)){
		message("getMting arqd data")
		
		return(inverse)
	}
	
	## Get the matrix from our object
	data <- y$getM()
	
	## Calculate the inverse of the matrix
	inverse <- solve(data)
	
	## Set the inverse to the object
	y$setInverse(inverse)

	## Return the matrix of the matrix
	return (inverse)
}
