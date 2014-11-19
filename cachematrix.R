
## The set of two functions computes or obtains from memory (if already available)
## an inverse of a matrix that was passed as input into makeCacheMatrix function. 
## The output of makeCacheMatrix serves as input into cacheSolve function.


## makeCacheMatrix function clears out a local variable m and creates a list of 4 functions.
## Each function is a named element in the list,
## and can be called via a reference to a list element (list_name$function_name)

makeCacheMatrix <- function(x) {
        m <- NULL
        ## local object y is assigned to object x (x is inside makeCacheMatrix function)
        ## object m (inside makeCacheMatrix function) is set to NULL
        
        setObj <- function(y) {
                        x <<- y
                        m <<- NULL
                }
        
        ## Returns object x. Since x is a free variable, the function will return
        ## object x from parent (makeCacheMatrix function) environemnt (if one exists) 
        ## or if not then from global environment
        
        getObj <- function() x
        
        ## Assigns its input to object m in parent environemnt (makeCacheMatrix function)
        
        setParam <- function(Param) m <<- Param
        
        ##Returns object m. Since m is a free variable, the function will return
        ## object m from parent (makeCacheMatrix function) environemnt (if one exists) 
        ## or if not then from global environment
        
        getParam <- function() m
        
        ## assigns 4 functions defined above to a list where names of the elements 
        ## match function names (helps readability)
        
        list(setObj = setObj, 
                getObj = getObj,
                setParam = setParam,
                getParam = getParam)
}


## cachSolve takes a list of functions (an output from makeCacheMatrix) as an input
## and checks whether an object m exists within makeCacheMatrix.
## if m exists within makeCacheMatrix (or global env), m returned as the result
## Otherwise, the function:
        ##obtains object x from makeCacheMatrix (or global env),
        ## computes inverse of x
        ## assign the result to m in makeCacheMatrix env
        ## returns inverse of x as the result


cacheSolve <- function(xList) {
                
        m <- xList$getParam()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- xList$getObj()
        m <- solve(data)
        xList$setParam(m)
        m
}        
