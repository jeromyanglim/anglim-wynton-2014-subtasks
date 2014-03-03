jags_overall <- function (f=c('power2', 'power3', 'exp3'), ymax=100) {
    f <- match.arg(f)
    
    # raw script
    script <- 
"model {
    # Model
    for (i in 1:length(y)) {
        $FUNCTION
        y[i]  ~ dnorm(mu[i], tau[subject[i]])
    }

    # Random coefficients
    for (i in 1:N) {    
        theta1[i] ~ dnorm(theta1.mu, theta1.tau)T(0, 1000)
        theta2[i] ~ dnorm(theta2.mu, theta2.tau)T(-10,  0)
        $THETA3DISTRIBUTION
        sigma[i] ~ dnorm(sigma.mu, sigma.tau)T(0, $YMAX);
        tau[i] <- 1/(sigma[i]^2)
    }

    
    theta1.mu  ~ dunif(0, $YMAX) 
    theta2.mu   ~ dunif(-2, 0)
    $THETA3PRIOR.MU
    sigma.mu ~ dunif(0, 20)

    theta1.sigma ~ dunif(0, $YMAX)
    theta2.sigma ~ dunif(0, 2)
    $THETA3PRIOR.SIGMA
    sigma.sigma ~ dunif(0, 10)
    
    # Transformations
    theta1.tau <- 1/(theta1.sigma^2)
    theta2.tau <- 1/(theta2.sigma^2)
    $THETA3.TAU 
    sigma.tau <- 1/(sigma.sigma^2)
}"


    # define macros
    macros <- list(list("$FUNCTION",  
          switch(f,
                 power2="mu[i] <- theta1[subject[i]] * pow(trial[i], theta2[subject[i]])", 
                 power3="mu[i] <- theta1[subject[i]] * pow(trial[i], theta2[subject[i]]) + theta3[subject[i]];",
                 exp3="mu[i] <- theta1[subject[i]] * exp(theta2[subject[i]] * (trial[i] - 1)) + theta3[subject[i]];") ), 
        list("$THETA3DISTRIBUTION",  
          switch(f, 
                 power3=, exp3= "theta3[i] ~ dnorm(theta3.mu, theta3.tau)T(0, 1000)", 
                 power2="") ), 
        list("$THETA3PRIOR.MU",  
          switch(f, 
                 power3=, exp3= "theta3.mu  ~ dunif(0, 100)", 
                 power2="") ), 
        list("$THETA3PRIOR.SIGMA",  
          switch(f, 
                 power3=, exp3= "theta3.sigma ~ dunif(0, 100)", 
                 power2="") ), 
        list("$THETA3.TAU",  
          switch(f, 
                 power3=, exp3= "theta3.tau <- 1/(theta3.sigma^2)", 
                 power2="") ),
        list("$YMAX", ymax)
        )

    # apply macros
    for (m in seq(macros)) {
        script <- gsub(macros[[m]][1], macros[[m]][2], script, fixed=TRUE)
    }
    script
}