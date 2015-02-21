## makeCacheMatrix creates a special matrix that can cache its inverse.

## great inspiration from https://class.coursera.org/rprog-009/forum/thread?thread_id=457
## Big help from poster Bill Hilton

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL 
        get<-function() {x}
        setSolve<-function(solve) {m<<-solve}
        getSolve<-function() {m}
        list(get=get, setSolve=setSolve, getSolve=getSolve)
}


## The function cacheSolve checks if inverse of 'x' has  been calculated
##if not, it will calculate and set the inverse.

cacheSolve <- function(x, ...) {
        #if already filled then return this.
        #if not, solve(naam van matrix)
        ## Return a matrix that is the inverse of 'x'
        m<- x$getSolve() #access the object x, and get value of solve.
        if(!is.null(m)){ # if solve was already cached (not NULL)
        message(" getting cached data") 
        return(m)  
        }
        data<- x$get()
        m<-solve(data, ...) #if m was NULL x is solved.
        x$setSolve(m) # store calculated solve in x, return solve in code that called this function.
}
