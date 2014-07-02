
simulate_data_subtasks_pve <- function(model='power', p) {
    N <- 25
    J <- 15
    K <- 3
    d <- expand.grid(subject=seq(N), trial = seq(J), subtask=seq(K))
    
    # simulate paramerters for each case
    cases <- data.frame(subject=seq(N))
    
    pname <- unique(sapply(strsplit(names(p), '.', fixed=TRUE), function(X) X[1]))
    for (i in seq(pname)) {
        p_alpha <- paste0(pname[i], '.', 'alpha')
        p_beta<- paste0(pname[i], '.', 'beta')
        p_mu<- paste0(pname[i], '.', 'mu')
        p_sigma<- paste0(pname[i], '.', 'sigma')
        
        p[[p_alpha]] <- p[[p_mu]]^2 / p[[p_sigma]]^2
        p[[p_beta]]  <-  p[[p_mu]] / p[[p_sigma]]^2
        cases[,pname[i]] <- rgamma(N, p[[p_alpha]] , p[[p_beta]])
    }
    d <- merge(d, cases)

    if (model == "power" ) {
        d$mu <- ifelse(d$subtask == 1, d$theta1_1 * d$trial ^ (0 - d$theta2_1) + d$theta3_1, 0) +
            ifelse(d$subtask == 2, d$theta1_2 * d$trial ^ (0 - d$theta2_2) + d$theta3_2, 0) +
            ifelse(d$subtask == 3, d$theta1_3 * d$trial ^ (0 - d$theta2_3) + d$theta3_3, 0)
    }
    if (model == "exp" ) {
        d$mu <- ifelse(d$subtask == 1, d$theta1_1 * exp(0 - d$theta2_1 * (d$trial - 1)) + d$theta3_1, 0) +
            ifelse(d$subtask == 2, d$theta1_2 * exp(0 - d$theta2_2 * (d$trial - 1)) + d$theta3_2, 0) +
            ifelse(d$subtask == 3, d$theta1_3 * exp(0 - d$theta2_3 * (d$trial - 1)) + d$theta3_3, 0)
    }
#       xyplot(mu ~ trial | factor(subject), d)
    d$alpha <- d$mu^2 / d$sigma^2
    d$beta <- d$mu / d$sigma^2
    d$y <- rgamma(nrow(d), d$alpha, d$beta)

#     d <- merge(d, meta.subtaskplot) # add meta data
#     cases
# 
#     d <- d[order(d$subject, d$trial, d$subtask), ]
#     xyplot(y~trial|factor(subject), d, groups=subtask, xlab = "Block", type='b', 
#            ylab="Subtask completion time (sec)", ylim = c(-5, 28), 
#            col= d$col, pch=d$pch, lty=d$lty)
# 
# xyplot(y ~ trial | factor(subject), d, group=subtask, ylim=c(0,40))
    list(d=d, cases=cases)    
}


simulate_data_subtasks_cus <- function(p, constraints=FALSE, strategies=FALSE, model='power') {
    N <- 25
    J <- 15
    K <- 3
    d <- expand.grid(subject=seq(N), trial = seq(J), subtask=seq(K))
    
    # simulate paramerters for each case
    cases <- data.frame(subject=seq(N))
    strategy_variables <- c('strategy_filter_level', 'strategy_questions_irrelevant_unique_mean', 
                            'strategy_accessrules', 
                            'strategy_filter_day', 'strategy_filter_level_off', 'strategy_filter_time')
    d_strategy <- rib[,c('user_id', 'block',  strategy_variables)]
    d_strategy$trial <- d_strategy$block
    d_strategy$subject <- as.numeric(factor(d_strategy$user_id))
    d <- merge(d, d_strategy)
    
    pname <- unique(sapply(strsplit(names(p), '.', fixed=TRUE), function(X) X[1]))
    for (i in seq(pname)) {
        p_alpha <- paste0(pname[i], '.', 'alpha')
        p_beta<- paste0(pname[i], '.', 'beta')
        p_mu<- paste0(pname[i], '.', 'mu')
        p_sigma<- paste0(pname[i], '.', 'sigma')
        
        p[[p_alpha]] <- p[[p_mu]]^2 / p[[p_sigma]]^2
        p[[p_beta]]  <-  p[[p_mu]] / p[[p_sigma]]^2
        cases[,pname[i]] <- rgamma(N, p[[p_alpha]] , p[[p_beta]])
    }

    
    # hack to rename lambdas for posterior predictive checks
    names(p)
    names(p) <- gsub("[", "", names(p), fixed=TRUE)
    names(p) <- gsub("]", "", names(p), fixed=TRUE)
    names(p) <- gsub(",", "_", names(p), fixed=TRUE)
    
    d <- merge(d, cases)
    
    if (model == "power" ) {
        if(constraints == FALSE) {
            d$mu <- ifelse(d$subtask == 1, d$theta1_1 * d$trial ^ (0 - d$theta2_1) + d$theta3_1 * 
                               ifelse(strategies,
                                      exp(as.matrix(d[, strategy_variables]) %*% 
                    c(p$lambda1_1, p$lambda2_1, p$lambda3_1,  p$lambda4_1, p$lambda5_1, p$lambda6_1)), 1)
                , 0) +
                ifelse(d$subtask == 2, d$theta1_2 * d$trial ^ (0 - d$theta2_2) + d$theta3_2 * 
                           ifelse(strategies,
                                  exp(as.matrix(d[, strategy_variables]) %*% 
                        c(p$lambda1_2, p$lambda2_2, p$lambda3_2,  p$lambda4_2, p$lambda5_2, p$lambda6_2)),1)
                    , 0) +
                ifelse(d$subtask == 3, d$theta1_3 * d$trial ^ (0 - d$theta2_3) + d$theta3_3 * 
                           ifelse(strategies,
                                  exp(as.matrix(d[, strategy_variables]) %*% 
                        c(p$lambda1_3, p$lambda2_3, p$lambda3_3,  p$lambda4_3, p$lambda5_3, p$lambda6_3)), 1)
                    , 0)        
        }
            
        if(constraints == TRUE) {
            d$mu <- ifelse(d$subtask == 1, d$theta1_1 * d$trial ^ (0 - d$theta2) + d$gamma * d$theta1_1 *           
                               ifelse(strategies,
                                      exp(as.matrix(d[, strategy_variables]) %*% 
                    c(p$lambda1_1, p$lambda2_1, p$lambda3_1,  p$lambda4_1, p$lambda5_1, p$lambda6_1)), 1)
                , 0) +
                ifelse(d$subtask == 2, d$theta1_2 * d$trial ^ (0 - d$theta2) + d$gamma * d$theta1_2 * 
                           ifelse(strategies, exp(
                    as.matrix(d[, strategy_variables]) %*% 
                        c(p$lambda1_2, p$lambda2_2, p$lambda3_2,  p$lambda4_2, p$lambda5_2, p$lambda6_2)), 1)
                    , 0) +
                ifelse(d$subtask == 3, d$theta1_3 * d$trial ^ (0 - d$theta2) + d$gamma * d$theta1_3 * 
                           ifelse(strategies, exp(
                    as.matrix(d[, strategy_variables]) %*% 
                        c(p$lambda1_3, p$lambda2_3, p$lambda3_3,  p$lambda4_3, p$lambda5_3, p$lambda6_3)), 1)
                    , 0)              
        }
        
    }
#     if (model == "exp" ) {
#         d$mu <- ifelse(d$subtask == 1, d$theta1_1 * exp(0 - d$theta2_1 * (d$trial - 1)) + d$theta3_1, 0) +
#             ifelse(d$subtask == 2, d$theta1_2 * exp(0 - d$theta2_2 * (d$trial - 1)) + d$theta3_2, 0) +
#             ifelse(d$subtask == 3, d$theta1_3 * exp(0 - d$theta2_3 * (d$trial - 1)) + d$theta3_3, 0)
#     }
#       xyplot(mu ~ trial | factor(subject), d)
    d$alpha <- d$mu^2 / d$sigma^2
    d$beta <- d$mu / d$sigma^2
    d$y <- rgamma(nrow(d), d$alpha, d$beta)

    list(d=d, cases=cases)    
}

model_recovery_subtaskscus <- function(K=10) {
    # 1. load  parameters
    p <- list()
    p$unconstrained <- meta.simulate.cus$unconstrained
    names(p$unconstrained) <- meta.simulate.cus$parameter
    p$unconstrained <- sapply(p$unconstrained, as.list)
    
    p$constrained <- meta.simulate.cus$constrained
    names(p$constrained) <- meta.simulate.cus$parameter
    p$constrained <- sapply(p$constrained, as.list)
    
    
    # 2. setup design and dataset data.frames
    design <- expand.grid(k=seq(K), generator=c('unconstrained', 'constrained'), 
                          estimator=1:4,
                          stringsAsFactors = FALSE)
    design$design_id <- seq(nrow(design))
    estimators <- expand.grid(constraints=c(TRUE, FALSE), strategies=c(TRUE, FALSE))
    estimators$estimator <- 1:4
    design <- merge(design, estimators)
    
    
    datasets <- expand.grid(k=seq(K), generator=c('unconstrained', 'constrained'),
                            stringsAsFactors = FALSE)
    datasets$datasets_id <- seq(nrow(datasets))
    design <- merge(design, datasets, sort=FALSE)
    design <- design[order(design$design_id), ]
    
    # 3. simulate data
    
    ds <- lapply(
        seq(nrow(datasets)), function(i) 
            simulate_data_subtasks_cus(p = p[[ datasets$generator[i] ]], 
                                       constraints=datasets$generator[i] == "constrained")$d
                                       
    )
    
    # and convert to JAGS format
    
    ds <- lapply(ds, function(X) get_data_subtasks_cus(X))
    
    
    # get bayesian estimates for each row of design (i.e., each estimator for each dataset)
    fits <- list()
    head(design)
    for (i in seq(nrow(design))) {
        cat(i, "of", nrow(design), "\n")
        jags_model <- jags_subtasks_gamma(f='power3', strategy_predictors=design$strategies[i], 
                                          theta2_constraint=design$constraints[i], gamma_constraint=design$constraints[i])
        jags_data <- ds[[ design$datasets_id[i] ]]
        fits[[i]] <- run_jags(jags_model$script, jags_data, jags_model$parameters, dic.run=TRUE, showplots=FALSE)
    }
    
    list(fits=fits, design=design, datasets=datasets, ds=ds)
}

    
model_recovery_subtaskspve <- function(K=10) {
    
    # loaded the parameters
    p <- list()
    p$exp <- meta.simulate.pve$exp
    names(p$exp) <- meta.simulate.pve$parameter
    p$exp <- sapply(p$exp, as.list)
    
    p$power <- meta.simulate.pve$power
    names(p$power) <- meta.simulate.pve$parameter
    p$power <- sapply(p$power, as.list)
    
    # setup design and dataset data.frames
    design <- expand.grid(k=seq(K), generator=c('exp', 'power'), 
                          estimator=c('exp', 'power'),
                          stringsAsFactors = FALSE)
    design$design_id <- seq(nrow(design))
    
    datasets <- expand.grid(k=seq(K), generator=c('exp', 'power'),
                            stringsAsFactors = FALSE)
    datasets$datasets_id <- seq(nrow(datasets))
    design <- merge(design, datasets, sort=FALSE)
    design <- design[order(design$design_id), ]
    
    # simulate our data
    ds <- lapply(
        seq(nrow(datasets)), function(i) 
            simulate_data_subtasks_pve(datasets$generator[i], 
                                       p[[ datasets$generator[i] ]])$d
    )
    ds <- lapply(ds, function(X) get_data_subtasks_pve(X))
    
    # get bayesian estimates for each row of design (i.e., each estimator for each dataset)
    fits <- list()
    for (i in seq(nrow(design))) {
        cat(i, "of", nrow(design), "\n")
        jags_model <- jags_subtasks_gamma(f=paste0(design$estimator[i], '3'), strategy_predictors=FALSE)
        jags_data <- ds[[ design$datasets_id[i] ]]
        fits[[i]] <- run_jags(jags_model$script, jags_data, jags_model$parameters, dic.run=TRUE, showplots=FALSE)
    }
    
    list(fits=fits, design=design, datasets=datasets, ds=ds)
}

    

    
    


get_data_subtasks <- function(Data = ribs[,c('user_id', 'block', 'subtask', 'rt', 'strategy_filter_level', 
                                             'strategy_questions_irrelevant_unique_mean', 'strategy_accessrules',
                                             'strategy_filter_day', 'strategy_filter_level_off', 'strategy_filter_time')]) {
    Data$user_id <- as.numeric(as.factor(Data$user_id))
    names(Data) <- c('subject', 'trial', 'subtask', 'y', 'strategy_filter_level', 'strategy_questions_irrelevant_unique_mean', 
                     'strategy_accessrules', 
                     'strategy_filter_day', 'strategy_filter_level_off', 'strategy_filter_time')
    
    # dummy code
    unique(Data$subtask)
    Data$subtask_1 <- as.numeric(Data$subtask == 'informationgathering')
    Data$subtask_2 <- as.numeric(Data$subtask == 'filtering')
    Data$subtask_3 <- as.numeric(Data$subtask == 'timetableuse')
    Data$subtask_index <- Data$subtask_1 + 2 * Data$subtask_2 + 3 * Data$subtask_3
    
    #             IG    FI     TT
    # subtask_1   1     0      0
    # subtask_2   0     1      0
    # subtask_3   0     0      1
    
    N <- length(unique(Data$subject))
    
    # Convert data to input format required by JAGS
    jagsdata <- list(subject=Data$subject, trial=Data$trial, 
                     subtask_1=Data$subtask_1, subtask_2=Data$subtask_2, subtask_3=Data$subtask_3,
                     strategy_filter_level=Data$strategy_filter_level,
                     strategy_questions_irrelevant_unique_mean=Data$strategy_questions_irrelevant_unique_mean,
                     strategy_accessrules=Data$strategy_accessrules,
                     strategy_filter_day = Data$strategy_filter_day , 
                     strategy_filter_level_off = Data$strategy_filter_level_off, 
                     strategy_filter_time = Data$strategy_filter_time,
                     subtask_index=Data$subtask_index,
                     y=Data$y, N=N)
    jagsdata$X <- with(jagsdata, cbind(strategy_filter_level, strategy_questions_irrelevant_unique_mean, strategy_accessrules,
                                       strategy_filter_time, strategy_filter_level_off, strategy_filter_day))
    jagsdata$y <- jagsdata$y + .01 # remove zeros
    jagsdata
}


get_data_subtasks_pve <- function(Data , user_id='subject', trial='trial', subtask='subtask', y='y') {
    
    Data$user_id <- as.numeric(as.factor(Data[,user_id]))
    Data <- Data[,c(user_id, trial, subtask, y)]
    names(Data) <- c('subject', 'trial', 'subtask', 'y')
    
    # dummy code
    Data$subtask_1 <- as.numeric(Data$subtask == 1) # informationgathering
    Data$subtask_2 <- as.numeric(Data$subtask == 2) # filtering
    Data$subtask_3 <- as.numeric(Data$subtask == 3) #timetableuse
    Data$subtask_index <- Data$subtask
    
    #             IG    FI     TT
    # subtask_1   1     0      0
    # subtask_2   0     1      0
    # subtask_3   0     0      1
    
    N <- length(unique(Data$subject))
    
    # Convert data to input format required by JAGS
    jagsdata <- list(subject=Data$subject, trial=Data$trial, 
                     subtask_1=Data$subtask_1, subtask_2=Data$subtask_2, subtask_3=Data$subtask_3,
                     subtask_index=Data$subtask_index,
                     X= matrix(0, nrow=length(y), ncol=2), # hack to get function to work without strategy
                     y=Data$y, N=N)
    jagsdata$y <- jagsdata$y + .01 # remove zeros
    jagsdata
}

get_data_subtasks_cus <- function(Data , user_id='subject', trial='trial', subtask='subtask', y='y') {
    
    strategy_variables <- c('strategy_filter_level', 'strategy_questions_irrelevant_unique_mean', 
      'strategy_accessrules', 
      'strategy_filter_day', 'strategy_filter_level_off', 'strategy_filter_time')
    Data$user_id <- as.numeric(as.factor(Data[,user_id]))
    Data <- Data[,c(user_id, trial, subtask, y, strategy_variables)]
    names(Data) <- c('subject', 'trial', 'subtask', 'y', strategy_variables)
    
    # dummy code
    Data$subtask_1 <- as.numeric(Data$subtask == 1) # informationgathering
    Data$subtask_2 <- as.numeric(Data$subtask == 2) # filtering
    Data$subtask_3 <- as.numeric(Data$subtask == 3) #timetableuse
    Data$subtask_index <- Data$subtask
    
    #             IG    FI     TT
    # subtask_1   1     0      0
    # subtask_2   0     1      0
    # subtask_3   0     0      1
    
    N <- length(unique(Data$subject))
    
    # Convert data to input format required by JAGS
    jagsdata <- list(subject=Data$subject, trial=Data$trial, 
                     subtask_1=Data$subtask_1, subtask_2=Data$subtask_2, subtask_3=Data$subtask_3,
                     subtask_index=Data$subtask_index,
                     X= Data[,strategy_variables],
                     y=Data$y, N=N)
    jagsdata$y <- jagsdata$y + .01 # remove zeros
    jagsdata
}

jags_subtasks_gamma <- function (f=c('power3', 'exp3'), 
            strategy_predictors = FALSE, theta2_constraint = FALSE, gamma_constraint = FALSE, yhat=FALSE) {
    f <- match.arg(f)

script <- 
"
data{
    D <- dim(X)
}
model {
    # Model
    for (ijk in 1:length(y)) {
        y[ijk]  ~ dgamma(alpha[ijk], beta[ijk])
        $YHAT
        alpha[ijk] <- mu[ijk]^2/sigma[subject[ijk]]^2
        beta[ijk] <- mu[ijk]/sigma[subject[ijk]]^2

        mu[ijk] <- subtask_1[ijk] * (theta1_1[subject[ijk]] * $FUNCTION_1 + $THETA3_1) $STRATEGIES_PARAMETERS_1 + 
             subtask_2[ijk] * (theta1_2[subject[ijk]] * $FUNCTION_2 + $THETA3_2) $STRATEGIES_PARAMETERS_2 +
             subtask_3[ijk] * (theta1_3[subject[ijk]] * $FUNCTION_3 + $THETA3_3) $STRATEGIES_PARAMETERS_3 

    }
        
    # Random coefficients
    for (i in 1:N) {    
        theta1_1[i] ~ dgamma(theta1_1.alpha, theta1_1.beta)
        theta1_2[i] ~ dgamma(theta1_2.alpha, theta1_2.beta)
        theta1_3[i] ~ dgamma(theta1_3.alpha, theta1_3.beta) 

        $THETA_2_RANDOM

        $THETA_3_RANDOM

        sigma[i] ~ dgamma(sigma.alpha, sigma.beta)
    }
        
    # priors
    theta1_1.mu ~ dunif(0, 50) 
    theta1_2.mu ~ dunif(0, 50) 
    theta1_3.mu ~ dunif(0, 50) 
    
    theta1_1.sigma ~ dunif(0, 50)
    theta1_2.sigma ~ dunif(0, 50)
    theta1_3.sigma ~ dunif(0, 50)

    $THETA_2_PRIOR

    $THETA_3_PRIOR

    $STRATEGY_PRIOR

    sigma.mu ~ dunif(0, 20)    
    sigma.sigma ~ dunif(0, 10) 

        
    # transformations
    theta1_1.alpha <- theta1_1.mu^2 / theta1_1.sigma^2
    theta1_1.beta <- theta1_1.mu/theta1_1.sigma^2
    theta1_2.alpha <- theta1_2.mu^2 / theta1_2.sigma^2
    theta1_2.beta <- theta1_2.mu/theta1_2.sigma^2
    theta1_3.alpha <- theta1_3.mu^2 / theta1_3.sigma^2
    theta1_3.beta <- theta1_3.mu/theta1_3.sigma^2

    $THETA_2_TRANSFORMATIONS

    $THETA_3_TRANSFORMATIONS

    sigma.alpha <- sigma.mu^2/sigma.sigma^2
    sigma.beta <- sigma.mu/sigma.sigma^2
}"



    # define macros
    macros <- list(
        list("$YHAT",
             ifelse(yhat,
                    "yhat[ijk] ~ dgamma(alpha[ijk], beta[ijk]",
                    "")
             ),
        list("$FUNCTION_1",  
            switch(f,
                power3="pow(trial[ijk], 0-$THETA2_1[subject[ijk]])",
                exp3="exp(0-$THETA2_1[subject[ijk]] * (trial[ijk] -1 )) ") 
            ),
        list("$FUNCTION_2",  
             switch(f,
                    power3="pow(trial[ijk], 0-$THETA2_2[subject[ijk]])",
                    exp3="exp(0-$THETA2_2[subject[ijk]] * (trial[ijk] -1 )) ") 
        ),
        list("$FUNCTION_3",  
             switch(f,
                    power3="pow(trial[ijk], 0-$THETA2_3[subject[ijk]])",
                    exp3="exp(0-$THETA2_3[subject[ijk]] * (trial[ijk] -1 )) ") 
        ),
        list("$THETA2_1",  
             ifelse(theta2_constraint, 
                    "theta2",
                    "theta2_1")
        ),
        list("$THETA2_2",  
             ifelse(theta2_constraint, 
                    "theta2",
                    "theta2_2")
        ),
        list("$THETA2_3",  
             ifelse(theta2_constraint, 
                    "theta2",
                    "theta2_3")
        ),
        list("$THETA3_1",  
             ifelse(gamma_constraint, 
                    "gamma[subject[ijk]] * theta1_1[subject[ijk]]",
                    "theta3_1[subject[ijk]]")
        ),
        list("$THETA3_2",  
             ifelse(gamma_constraint, 
                    "gamma[subject[ijk]] * theta1_2[subject[ijk]]",
                    "theta3_2[subject[ijk]]")
        ),
        list("$THETA3_3",  
             ifelse(gamma_constraint, 
                    "gamma[subject[ijk]] * theta1_3[subject[ijk]]",
                    "theta3_3[subject[ijk]]")
        ),
        list("$STRATEGIES_PARAMETERS_1",  
             ifelse(strategy_predictors, 
                    "* exp(inprod(lambda[,1], X[ijk, ]))",
                    "")
        ),
        list("$STRATEGIES_PARAMETERS_2",  
             ifelse(strategy_predictors, 
                    "* exp(inprod(lambda[,2], X[ijk, ]))",
                    "")
        ),
        list("$STRATEGIES_PARAMETERS_3",  
             ifelse(strategy_predictors, 
                    "* exp(inprod(lambda[,3], X[ijk, ]))",
                    "")
        ),
        
        list("$THETA_2_RANDOM",  
             ifelse(theta2_constraint, 
                    "theta2[i] ~ dgamma(theta2.alpha, theta2.beta)",
                    "theta2_1[i] ~ dgamma(theta2_1.alpha, theta2_1.beta)
    theta2_2[i] ~ dgamma(theta2_2.alpha, theta2_2.beta)
    theta2_3[i] ~ dgamma(theta2_3.alpha, theta2_3.beta)")
        ),
        list("$THETA_3_RANDOM",  
             ifelse(gamma_constraint, 
                    "gamma[i] ~ dgamma(gamma.alpha, gamma.beta)",
                    "theta3_1[i] ~ dgamma(theta3_1.alpha, theta3_1.beta)
    theta3_2[i] ~ dgamma(theta3_2.alpha, theta3_2.beta)
    theta3_3[i] ~ dgamma(theta3_3.alpha, theta3_3.beta)")
        ),
        list("$THETA_2_PRIOR",  
             ifelse(theta2_constraint,  
                    "theta2.mu ~ dunif(0, 2)
    theta2.sigma ~ dunif(0, 2)",
                    "theta2_1.mu ~ dunif(0, 2)
    theta2_2.mu ~ dunif(0, 2)
    theta2_3.mu ~ dunif(0, 2)
    theta2_1.sigma ~ dunif(0, 2)
    theta2_2.sigma ~ dunif(0, 2)
    theta2_3.sigma ~ dunif(0, 2)")
             ),
        list("$THETA_3_PRIOR",  
             ifelse(gamma_constraint, 
                    "gamma.mu ~ dunif(0, 5)
    gamma.sigma ~ dunif(0, 5)",
                    "theta3_1.mu ~ dunif(0, 30)
    theta3_2.mu ~ dunif(0, 30)
    theta3_3.mu ~ dunif(0, 30)

    theta3_1.sigma ~ dunif(0, 30)
    theta3_2.sigma ~ dunif(0, 30)
    theta3_3.sigma ~ dunif(0, 30)")
             ),
        list("$STRATEGY_PRIOR",  
             ifelse(strategy_predictors, 
                "for (p in 1:D[2]) { # predictor
        for (k in 1:3) { # subtask
            lambda[p,k] ~ dnorm(0,1)
            
        }
    }", 
                "")
        ),
        list("$THETA_2_TRANSFORMATIONS",  
             ifelse(theta2_constraint,
                    "theta2.alpha <- theta2.mu^2/theta2.sigma^2
    theta2.beta <- theta2.mu/theta2.sigma^2",
                    "theta2_1.alpha <- theta2_1.mu^2/theta2_1.sigma^2
    theta2_1.beta <- theta2_1.mu/theta2_1.sigma^2
    theta2_2.alpha <- theta2_2.mu^2/theta2_2.sigma^2
    theta2_2.beta <- theta2_2.mu/theta2_2.sigma^2
    theta2_3.alpha <- theta2_3.mu^2/theta2_3.sigma^2
    theta2_3.beta <- theta2_3.mu/theta2_3.sigma^2")
        ),
        list("$THETA_3_TRANSFORMATIONS",  
             ifelse(gamma_constraint,
                    "gamma.alpha <- gamma.mu^2/gamma.sigma^2
    gamma.beta <- gamma.mu/gamma.sigma^2",
                    "theta3_1.alpha <- theta3_1.mu^2/theta3_1.sigma^2
    theta3_1.beta <- theta3_1.mu/theta3_1.sigma^2
    theta3_2.alpha <- theta3_2.mu^2/theta3_2.sigma^2
    theta3_2.beta <- theta3_2.mu/theta3_2.sigma^2
    theta3_3.alpha <- theta3_3.mu^2/theta3_3.sigma^2
    theta3_3.beta <- theta3_3.mu/theta3_3.sigma^2")
        )

                    
        
    )
    
    # apply macros
    for (m in seq(macros)) {
        script <- gsub(macros[[m]][1], macros[[m]][2], script, fixed=TRUE)
    }

    # return parameters
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
                    c("lambda")
                } else { 
                    NULL
                },
                'sigma.mu', 'sigma.sigma')
    list(script=script, parameters = parameters)
}

