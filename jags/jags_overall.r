jags_overall <- function (f=c('power2', 'power3', 'exp3')) {
    f <- match.arg(f)
    paste(
"model {
    # Model
    for (i in 1:length(y)) {
      ",ifelse(f=="power2", "mu[i] <- theta1[subject[i]] * pow(trial[i], theta2[subject[i]]);", ""),
        ifelse(f=="power3", "mu[i] <- theta1[subject[i]] * pow(trial[i], theta2[subject[i]]) + theta3[subject[i]];", ""),
        ifelse(f=="exp3", "mu[i] <- theta1[subject[i]] * exp(theta2[subject[i]] * (trial[i] - 1)) + theta3[subject[i]];", ""),"
        y[i]  ~ dnorm(mu[i], tau[subject[i]])
    }


    # Random coefficients
    for (i in 1:N) {    
        theta1[i] ~ dnorm(theta1.mu, theta1.tau)T(0, 1000)
        theta2[i] ~ dnorm(theta2.mu, theta2.tau)T(-10,  0)
      ",ifelse(f %in% c("power3", "exp3"), "theta3[i] ~ dnorm(theta3.mu, theta3.tau)T(0, 1000);", ""), "
        sigma[i] ~ dnorm(sigma.mu, sigma.tau)T(0, 100);
        tau[i] <- 1/(sigma[i]^2)
    }

    
    theta1.mu  ~ dunif(0, 100)
    theta2.mu   ~ dunif(-2, 0)
  ",ifelse(f %in% c("power3", "exp3"), "theta3.mu  ~ dunif(0, 100)", ""), "
    sigma.mu ~ dunif(0, 20)
    theta1.sigma ~ dunif(0, 100)
    theta2.sigma ~ dunif(0, 2)
  ",ifelse(f %in% c("power3", "exp3"), "theta3.sigma ~ dunif(0, 100)", ""),"    
    sigma.sigma ~ dunif(0, 10)
    
    # Transformations
    theta1.tau <- 1/(theta1.sigma^2)
    theta2.tau <- 1/(theta2.sigma^2)
  ",ifelse(f %in% c("power3", "exp3"), "theta3.tau <- 1/(theta3.sigma^2)", ""),"   
    sigma.tau <- 1/(sigma.sigma^2)
}"
)
}