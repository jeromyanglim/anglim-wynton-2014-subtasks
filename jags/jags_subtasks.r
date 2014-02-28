jags_subtasks <- function (f=c('power3', 'exp3'), 
            strategy_predictors = FALSE, theta2_constraint = FALSE, gamma_constraint = FALSE) {
    f <- match.arg(f)
    function_name <- ifelse(f=='power3', 'pow', '')
    function_name <- ifelse(f=='exp3', 'exp', function_name)
    trial_argument <- ifelse(f=='power3', 'trial[i], ', '')
    trial_argument <- ifelse(f=='exp3', '(trial[i] - 1) *', trial_argument)                        

    theta2 <- list()
    theta2[[1]] <- ifelse(theta2_constraint, 'theta2', 'theta2_1')
    theta2[[2]] <- ifelse(theta2_constraint, 'theta2', 'theta2_2')
    theta2[[3]] <- ifelse(theta2_constraint, 'theta2', 'theta2_3')

    theta3 <- list()
    theta3[[1]] <- ifelse(gamma_constraint, "gamma[subject[i]] * theta1_1[subject[i]]", "theta3_1[subject[i]]")
    theta3[[2]] <- ifelse(gamma_constraint, "gamma[subject[i]] * theta1_2[subject[i]]", "theta3_2[subject[i]]")
    theta3[[3]] <- ifelse(gamma_constraint, "gamma[subject[i]] * theta1_3[subject[i]]", "theta3_3[subject[i]]")
    
    betas <- list()
    betas[[1]] <-  ifelse(strategy_predictors, "+ inprod(beta[,1], X[i, ])", "")
    betas[[2]] <-  ifelse(strategy_predictors, "+ inprod(beta[,2], X[i, ])", "")
    betas[[3]] <-  ifelse(strategy_predictors, "+ inprod(beta[,3], X[i, ])", "")
    
    
script <- paste(
    ifelse(strategy_predictors,
        "
data{
D <- dim(X)
}", "")
    
,"model{
    # Model
    for (i in 1:length(y)) {
        ",sprintf(
            "mu[i] <- subtask_1[i] * (theta1_1[subject[i]] * %1$s(%2$s %6$s[subject[i]]) + %3$s %9$s)+
            subtask_2[i] * (theta1_2[subject[i]] * %1$s(%2$s %7$s[subject[i]]) +  %4$s %10$s) +
            subtask_3[i] * (theta1_3[subject[i]] * %1$s(%2$s %8$s[subject[i]]) +  %5$s %11$s)",
                    function_name, trial_argument, 
            theta3[[1]], theta3[[2]], theta3[[3]], 
            theta2[[1]], theta2[[2]], theta2[[3]],
            betas[[1]], betas[[2]], betas[[3]]),
"
        y[i]  ~ dnorm(mu[i], tau[subject[i]])
}
        
        # Random coefficients
        for (i in 1:N) {    
        theta1_1[i] ~ dnorm(theta1_1.mu, theta1_1.tau)
        theta1_2[i] ~ dnorm(theta1_2.mu, theta1_2.tau)
        theta1_3[i] ~ dnorm(theta1_3.mu, theta1_3.tau) 
        
        ",ifelse(theta2_constraint,
"       theta2[i] ~ dnorm(theta2.mu, theta2.tau)",

"       theta2_1[i] ~ dnorm(theta2_1.mu, theta2_1.tau)
        theta2_2[i] ~ dnorm(theta2_2.mu, theta2_2.tau)
        theta2_3[i] ~ dnorm(theta2_3.mu, theta2_3.tau)"),
"
        ",ifelse(gamma_constraint,
"       gamma[i] ~ dnorm(gamma.mu, gamma.tau)",
"       theta3_1[i] ~ dnorm(theta3_1.mu, theta3_1.tau)
        theta3_2[i] ~ dnorm(theta3_2.mu, theta3_2.tau)
        theta3_3[i] ~ dnorm(theta3_3.mu, theta3_3.tau)"
        ), 

"       
        sigma[i] ~ dnorm(sigma.mu, sigma.tau)T(0, 100)
        tau[i] <- 1/(sigma[i]^2)
        
        }
        
        # Priors
        theta1_1.mu  ~ dunif(0, 100)
        theta1_2.mu   ~dunif(0, 100)
        theta1_3.mu   ~ dunif(0, 100)
        theta1_1.sigma ~ dunif(0, 100)
        theta1_2.sigma ~ dunif(0, 100)
        theta1_3.sigma ~ dunif(0, 100)
        theta1_1.tau  <- 1.0/(theta1_1.sigma^2);
        theta1_2.tau  <- 1.0/(theta1_2.sigma^2);
        theta1_3.tau  <- 1.0/(theta1_3.sigma^2);

        ",ifelse(theta2_constraint,
       "theta2.mu  ~ dunif(-1, 0)
        theta2.sigma ~ dunif(0, 2)
        theta2.tau  <- 1.0/(theta2.sigma^2);",
       
       "theta2_1.mu  ~ dunif(-1, 0)
        theta2_2.mu   ~ dunif(-1, 0)
        theta2_3.mu   ~ dunif(-1, 0)
        theta2_1.sigma ~ dunif(0, 2)
        theta2_2.sigma ~ dunif(0, 2)
        theta2_3.sigma ~ dunif(0, 2)
       theta2_1.tau  <- 1.0/(theta2_1.sigma^2);
        theta2_2.tau  <- 1.0/(theta2_2.sigma^2);
        theta2_3.tau  <- 1.0/(theta2_3.sigma^2);
"),
"

        ",ifelse(gamma_constraint,
        "gamma.mu ~ dunif(0, 5)
        gamma.sigma ~ dunif(0, 2)
        gamma.tau <- 1/(gamma.sigma^2)",
        
        "theta3_1.mu  ~ dunif(0, 100)
        theta3_2.mu   ~ dunif(0, 100)
        theta3_3.mu   ~ dunif(0, 100)
        theta3_1.sigma ~ dunif(0, 100)
        theta3_2.sigma ~ dunif(0, 100)
        theta3_3.sigma ~ dunif(0, 100)        
        theta3_1.tau  <- 1.0/(theta3_1.sigma^2);
        theta3_2.tau  <- 1.0/(theta3_2.sigma^2);
        theta3_3.tau  <- 1.0/(theta3_3.sigma^2);"),
        
"       
        sigma.mu ~ dunif(0, 10)
        sigma.sigma ~ dunif(0, 10)
        sigma.tau <- 1/ (sigma.sigma^2)

    ",ifelse(strategy_predictors,
"    for (j in 1:D[2]) { # predictor
        for (k in 1:3) { # subtask
            beta[j,k] ~ dunif(-20,20)
        }
    }", ""),"
}")


    parameters <- c('theta1_1.mu', 'theta1_2.mu', 'theta1_3.mu',
                'theta1_1.sigma', 'theta1_2.sigma', 'theta1_3.sigma',
                if (theta2_constraint) {
                    c('theta2.mu', 'theta2.sigma')
                } else {
                    c('theta2_1.mu', 'theta2_2.mu', 'theta2_3.mu','theta2_1.sigma', 'theta2_2.sigma', 'theta2_3.sigma')
                },
                if (gamma_constraint) {
                    c('gamma.mu', 'gamma.sigma')
                } else {
                    c('theta3_1.mu', 'theta3_2.mu', 'theta3_3.mu',
                        'theta3_1.sigma', 'theta3_2.sigma', 'theta3_3.sigma')
                },
                if (strategy_predictors) {
                    c("beta")
                } else { 
                    NULL
                },
                    'sigma.mu', 'sigma.sigma')
    
    list(script=script, parameters=parameters)

}

