# This function performs sample size calculation for a general linear model
#
# Args:
#   u: Degrees of freedom for numerator
#   R2: Coefficient of determination
#   power: Power of test
#   sig.level: Significance level
#
# Returns:
#   size: The calculated sample size

sample_size_wif <- function(u, R2, power, sig.level) {
  # Calculate the effect size f2
  f2 <- R2 / (1 - R2)
  
  # Perform the test for the general linear model
  result <- pwr.f2.test(
    u = u,
    f2 = f2,
    sig.level = sig.level,
    power = power
  )
  
  # Calculate the sample size
  size <- ceiling(result$v) + u + 1
  
  # Return the sample size
  return(size)
}
