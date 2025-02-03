## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Variável para armazenar a inversa da matriz
  
  # Função para definir a matriz
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Limpa o cache quando a matriz é redefinida
  }
  
  # Função para obter a matriz
  get <- function() x
  
  # Função para definir a inversa da matriz
  setInverse <- function(inverse) inv <<- inverse
  
  # Função para obter a inversa da matriz
  getInverse <- function() inv
  
  # Retorna uma lista de funções
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
    }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  # Se a inversa já foi calculada, retorna do cache
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Obtém a matriz original
  mat <- x$get()
  
  # Calcula a inversa
  inv <- solve(mat, ...)
  
  # Armazena a inversa no cache
  x$setInverse(inv)
  
  # Retorna a inversa
  inv}

