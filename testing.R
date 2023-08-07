factorial = function(n) {
  if (n == 1)
    return(1)
  else 
    return(factorial(n-1) *n)
}
factorial(3)
factorial(5)
