## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
I<-NULL
set<-function(y){
  x<<-y
  I<<-NULL
} ## 1-This first chunk of code is defining the parent environment "makeCacheMatrix" whereby the matrix object x is the single arguement passed.##
  ## 2-Next, the defining environment "set" takes the matrix object defined in the parent environment and assigns it to the defining environment's arguement; If there is already
  ##a valid matrix inverse cached in I, whenever the matrix object needs to be reset the value of I cached in the memory is cleared forcing CacheSolve to calculate a new inverse##
  ##In other words "set" is the function that will re-set the matrix object as needed##

get<-function() x 
  setInverse<-function(Inverse) I<<-Inverse 
  getInverse<-function() I
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

} ##This last chunk does the following;##
## "Get" is the getter funtion of x. Because of Lexical scoping in R, since x is not defined within the get function environment, R scopes it from the 
##parent environment of makeCacheMatrix##
## "SetInverse" utilizing lexical scoping in R; given that I is initialized in the parent environment and given that we need to access after setInverse has executed; we use
##the assignment operator '<<-' which will cache the value of the Inverse##
##"getInverse" this is the last 'getter' function as it relates to the actual calculation of the matrix inverse##
##list containing all the 'setters and getters' of the matrix and its inverse; this list returns a fully formed object of type 'makeCacheMatrix()'.  Further because each
##object in the list is named, it can be accessed by name via the $ operator to access each of the functional parts within the parent enviornment of 'makeCacheMatrix'##


## Below the cacheSolve function takes an input of type 'makecacheMatrix' defined through 'x' as well as other arguements defined in the parent environment of makecacheMatrix
##defined through the '...' 'cacheSolve', checks to see if the current stored value of 'getInverse' is NULL.  If it is not NULL then it automatically returns the stored (cached)
##value. If it is NULL then it goes on to calculate the inverse of that matrix via the solve function and sets that as te new inverse via calling the 'setInverse' function.

cacheSolve <- function(x, ...) {
  #Return a matrix that is the inverse of x
 I<-x$getInverse()
 if(!is.null(I)){
   message("getting cached data")
   return(I)
 }
 data.matrix<-x$get()
 I<-solve(data.matrix,...)
 x$setInverse(I)
 I
}
