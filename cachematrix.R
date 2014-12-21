## This function catches with the <<- operator. That way the enviroments can be shilded.
## Is does it by set, get, matrixx and get_matrixx
makeCacheMatrix <- function(x = matrix()) ## catching{
  m_first<-NULL #storing the matrix
  set<-function(y){
    x<<-y
    m_first<<-NULL
  }
  get<-function() x #return input function
  matrixx<-function(solve) m_first<<- solve #set
  get_matrix<-function() m_first #get
  list(set=set, get=get,
       matrixx=matrixx,
       get_matrix=get_matrix)
}

cacheSolve <- function(x=matrix(), ...) ## getting the matrix{
  m_first<-x$get_matrix()
  if(!is.null(m_first)){
    message("getting cached data")
    return(m_first)
  }
  matrix<-x$get()
  m_first<-solve(matrix, ...)
  x$matrixx(m_first)
  m_first
}