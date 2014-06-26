jags_overall_gamma <- function (f=c('power2', 'power3', 'exp3'), ymax=100) {
    f <- match.arg(f)
    
    # raw script
    script <- 
"model {
    # Model
    for (ij in 1:length(y)) {
        
        y[ij]  ~ dgamma(alpha[ij], beta[ij])
        yhat[ij]  ~ dgamma(alpha[ij], beta[ij])
        alpha[ij] <- mu[ij]^2/sigma[subject[ij]]^2
        beta[ij] <- mu[ij]/sigma[subject[ij]]^2
        $FUNCTION
        # mu[ij] <- theta1[subject[ij]] * pow(trial[ij], 0-theta2[subject[ij]]) + theta3[subject[ij]]
    }

    # Random coefficients
    for (i in 1:N) {    
        theta1[i] ~ dgamma(theta1.alpha, theta1.beta)
        theta2[i] ~ dgamma(theta2.alpha, theta2.beta)
        # $THETA3DISTRIBUTION
        theta3[i] ~ dgamma(theta3.alpha, theta3.beta)
        sigma[i] ~ dgamma(sigma.alpha, sigma.beta)
    }

    # priors
    theta1.mu ~ dunif(0, 50) 
    theta2.mu ~ dunif(0, 2)
    theta3.mu ~ dunif(0, 30)
    # $THETA3PRIOR.MU
    sigma.mu ~ dunif(0, 20)

    theta1.sigma ~ dunif(0, 50)
    theta2.sigma ~ dunif(0, 2)
    theta3.sigma ~ dunif(0, 30)
    # $THETA3PRIOR.SIGMA
    sigma.sigma ~ dunif(0, 10)
 
    # transformations
       theta1.alpha <- theta1.mu^2 / theta1.sigma^2
        theta1.beta <- theta1.mu/theta1.sigma^2

        theta2.alpha <- theta2.mu^2/theta2.sigma^2
        theta2.beta <- theta2.mu/theta2.sigma^2
    
        sigma.alpha <- sigma.mu^2/sigma.sigma^2
        sigma.beta <- sigma.mu/sigma.sigma^2

        theta3.alpha <- theta3.mu^2/theta3.sigma^2
        theta3.beta <- theta3.mu/theta3.sigma^2
}"


     # define macros
     macros <- list(list("$FUNCTION",  
           switch(f,
                  power3="mu[ij] <- theta1[subject[ij]] * pow(trial[ij], 0 - theta2[subject[ij]]) + theta3[subject[ij]];",
                  exp3="mu[ij] <- theta1[subject[ij]] * exp(0 - theta2[subject[ij]] * (trial[ij] - 1)) + theta3[subject[ij]];") )
         )
 
     # apply macros
     for (m in seq(macros)) {
         script <- gsub(macros[[m]][1], macros[[m]][2], script, fixed=TRUE)
     }
    script
}
