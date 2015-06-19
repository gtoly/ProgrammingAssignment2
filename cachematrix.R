## Cached matrix solve

# Make cached matrix (for cache inversions)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  mat<-x
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
  list(get=get,set=set,get_inv=get_inv,set_inv=set_inv)
}


# Return inversed matrix (cached calculation)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$get_inv()
  if (!is.null(m)){
    message("get cache")
    return(m)
  }
  m<-solve(x$get())
  x$set_inv(m)
  m
}
