makeCacheMatrix <- function(x = matrix()) {  
	s <- NULL			## Reserves s with a NULL value
        set <- function(y) {		## Assigns a function
                x <<- y			## Assigns a value to a an object which is in a different enviroment
                s <<- NULL		## Assigns an empty value to a an object which is in a different enviroment
        }
        get <- function() x 		## Assigns a function with a variable
        setsolved <- function(solve) s <<- solve	 ## Assigns a function with a variable; pass the name of the function to an object which is in a different enviroment
        getsolved <- function() s	## Assigns a function with a variable
        list(set = set, get = get,	## Sets a list 
             setsolved = setsolved,
             getsolved = getsolved)
}





cacheSolve <- function(x, ...) {
	s <- x$getsolved()	## Assing value of inversed matrix x to variable s 
        if(!is.null(s)) {	## Checks if s is not a NULL value
                message("getting cached data") ## If s is not a NULL print a message "getting cached data"
                return(s)	## If s is not a NULL returns s 
        }
        data <- x$get()		## If s is a NULL getting a matrix x and assigns it to data
        s <- solve(data, ...)	## Solves matrix named data
        x$setsolved(s)		## Sets solved matrix
        s        		## Return a matrix that is the inverse of 'x'
}
