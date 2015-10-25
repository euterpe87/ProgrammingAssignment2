## initialize the matrix as ex. 
## mymatrix <- makeCacheMatrix(matrix(1:4,2,2))
## run cacheSolve(mymatrix) for the first time: it will not find
## the inverse of mymatrix in the cache and thus it will
## compute it
## run cacheSolve(mymatrix) for a second time: it will find the
## inverse matrix "inv" in the cache and return it

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
  	set <- function(y){
  		x <<- y
  		inv <<- NULL
		}
  	get <- function() x
	invertmatrix <- function(solve) {
		inv <<- solve
		}
	getinverse <- function() inv
	list(	set = set, 
		get = get,
   		invertmatrix = invertmatrix,
   		getinverse = getinverse
		)
}

cacheSolve <- function(x = matrix(), ...) {
	## Return a matrix that is the inverse of 'x'
    	inv <- x$getinverse()
    	if(!is.null(inv)){
      	message("getting cached inverse matrix")
      	return(inv)
    		}
    	matrix <- x$get()
    	inv <- solve(matrix, ...)
    	x$invertmatrix(inv)
    	inv
}