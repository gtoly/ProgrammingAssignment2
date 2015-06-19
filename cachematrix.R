## Cached matrix solve

## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL # cached invertion
  mat<-x    # the matrix itself
  
  # object methods
  get<-function(){
    mat 
  }
  set<-function(matrix_new){
    mat<<-matrix_new
    inv<<-NULL
  }
  get_inv<-function(){
    inv
  }
  set_inv<-function(new_inv){
    inv<<-new_inv
  }
  
  # "matrix" object
  list(get=get,set=set,get_inv=get_inv,set_inv=set_inv)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  m<-x$get_inv()
  if (!is.null(m)){ # return cached result
    return(m)
  }
  m<-solve(x$get())
  x$set_inv(m)  # remember result for future use
  m
}
