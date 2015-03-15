################################################################################
## There are two functions defined here. Together the two functions calculate 
## the inverse of a non-zero and invertible matrix and save it for future use. 
## For a given matrix, the makeCacheMatrix() needs to be called only once and 
## assigned to a variable. Calling the cacheSolve() on this variable for the 
## first time will calculate the inverse, change the value of the inverse in the
## makeCacheMatrix() environment, and return the inverse Matrix. 
## Future callings of cacheSolve() on the variable will access the saved value  
## from makeCacheMatrix() environment along with a message that the value was 
## previously calculated. Every time an inverse of a new matrix needs to be 
## calculated, the entire process is repeated.  
################################################################################



## makeCacheMatrix() takes a square matrix as its argument and returns a list of  
## three functions. getInverse() returns the inverse saved in this environment,
## get() returns the matrix, and setInverse() redefines the value of the inverse
## I. At the begining I is defined to be null. 

makeCacheMatrix <- function(x = matrix()){
	I <- NULL
	getInverse <- function() I
	get <- function() x
	setInverse <- function(inverse){
		I <<- inverse  #reassigning I in the makeCacheMatrix() environment
	}
	list(get = get, getInverse= getInverse,
	setInverse = setInverse) 
}


## cacheSolve() takes the list of functions defined by makeCacheMatrix() above
## and access the inverse I. If I is already calculated it returns that value,
## otherwise it calculates I, save it in using setInverse() and return I. 
cacheSolve <- function(x,...){
	I <- x$getInverse()
	if(!is.null(I)){
		message("Inverse previously calculated, returing cached value")
		return(I)
	}
	data <- x$get()
	I <- solve(data)  # calculating the inverse of the matrix 
	x$setInverse(I) # saving I in makeCacheMatrix environment
	I 
	
}