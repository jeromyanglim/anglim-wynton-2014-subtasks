run_jags <- function(script, data, variable.names, dic.run=FALSE,
                     n.chains=settings$n.chains, n.adapt=settings$n.adapt, burnin=settings$burnin, 
                     n.iter=settings$n.iter, thin=settings$thin, dic.type='pD') {  
    # if script is file name, then import as file name
    if(file.exists(script))  script <-  paste( readLines(script, warn=FALSE) , collapse="\n")
    cat('jags.model: \n')
    mod <- jags.model(textConnection(script), data=data,  n.chains=n.chains, n.adapt=n.adapt)
    cat('update: \n')
    update(mod, n.iter=n.iter) # burn in
    cat('coda.samples: \n')
    samples <- coda.samples(model=mod, n.iter=n.iter, thin=thin, variable.names=variable.names)
    
    plot(samples, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
    print(summary(samples))
    
    cat('dic.samples: \n')
    dic <- NULL
    if (dic.run) {
        dic <- dic.samples(mod, n.iter=n.iter, thin=thin, type=dic.type) # Deviance Information Criterion    
    }
    list(mod=mod, samples=samples, dic=dic, data=data, script=script, variable.names=variable.names)
}

summary_table_posterior <- function(mcmc_object, parameters=row.names(summary(mcmc_object)$statistics),
                        statistics=c('Mean', '2.5%', '97.5%'), digits=2,
                                    collapse=FALSE) {
    s <- summary(mcmc_object)
    s <- cbind(rbind(s$statistics), rbind(s$quantiles))
    snames <-  dimnames(mcmc_object[[1]])[[2]]
    row.names(s) <- snames
    if (length(setdiff(parameters, snames)) > 0) {
        stop(paste("these parameters not in mcmc_object:", setdiff(parameters, row.names(s))))
    }
    sb <- s[parameters,statistics, drop=FALSE]
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    x <- trim(format(round(sb, digits), nsmall = digits))
    if (collapse) {
        x <- cbind(  apply(x, 1, function(X) paste0(X['Mean'], " (", X['2.5%'], ", ", X["97.5%"], ")")))
    }
    x
}

plot_posterior_density <- function(mcmc_object, 
    variable, ...) {
    x <- as.vector(sapply(mcmc_object, function(X) X[, variable]))
    plot(density(x), main="", ...) 
}

simulate_many <- function(thetas, FUN) {
    lapply(seq(nrow(thetas)), function(X)  FUN(thetas[X, ]))
}

calculate_statistics <- function(simulations, dataset=Data) {
    stats <- function (x) {
        # x: single dataset
        x$w <- as.logical(x$w)
        list(cor = cor(x$y_d, x$y_a), 
             mean_y_d = mean(x$y_d), 
             mean_y_a = mean(x$y_a), 
             sd_y_d = sd(x$y_d),
             sd_y_a = sd(x$y_a),
             y_a_gt_y_d = mean(x$y_a > x$y_d),
             y_a_lt0.5sd_y_d = mean(x$y_a < (x$y_d - 0.5 * sd(x$y_d))),
             y_a_gt0.5sd_y_d = mean(x$y_a > (x$y_d + 0.5 * sd(x$y_d))),
             y_a_gt1sd_y_d = mean(x$y_a > (x$y_d + 1 * sd(x$y_d))),
             u_diff_minus_w_diff =  mean(x$y_a[!x$w] - x$y_d[!x$w]) -
                 mean(x$y_a[x$w] - x$y_d[x$w]) 
        )
    }
    simulations_stats <- sapply(simulations, stats)
    dataset_stats <- stats(Data)
    list(simulations=simulations_stats, dataset=dataset_stats)
}

summarise_statistics <- function(x) {
    # x: object returned from calculate statistics
    for_one_theta <- function(theta_i) {
        d <- x$dataset[[theta_i]]
        s <- unlist(x$simulations[theta_i, ])
        pvalue <- min(mean(d < s), 
                      mean(d > s)) * 2
        meanvalue <- mean(s)
        sdvalue <- sd(s)
        low95value <- as.numeric(quantile(s, .025))
        high95value <- as.numeric(quantile(s, .975))
        c(dataset=d, pvalue=pvalue, 
             mean=meanvalue, sd=sdvalue, 
             low95 =low95value,
             high95 = high95value)
    }
    
    thetas <-  names(x$dataset)
    sapply(thetas, for_one_theta)
}

get_dic <- function(x) {
    # x is dic object
    # returns deviance, penalty, and penalised deviance
    deviance <- sum(x$deviance)
    psum <- sum(x[[2]])
    penalised_deviance <- deviance + psum
    c(deviance=deviance, penalty=psum, penalised_deviance=penalised_deviance)
}

create_parameter_table <- function(samples, dics, parameter_order) {
    # samples: list of MCMC samples of parameters (length = to number of models)
    # dics: list of DIC samples from MCMC (length = to number of models)
    # parameter_order: vector of named parameter in desired order
    parameters <- lapply(samples, function(X) data.frame(summary_table_posterior(X, collapse=TRUE)))
    # naming variables is necessary for the subsequent merging
    for(i in seq(parameters)) {
        names(parameters[[i]]) <- names(parameters)[i]
    }

    parameter_table <- merge(parameters[[1]], parameters[[2]], by=0, all=TRUE)
    if (length(parameters) > 2) {
        for (i in seq(3, length(parameters))) {
            parameter_table <- merge(parameter_table, parameters[[i]], 
                                     by.x='Row.names', by.y='row.names', all=TRUE)
        }
    }
    names(parameter_table) <- c('parameter', names(samples))
    
    row.names(parameter_table) <- parameter_table$parameter
    
    parameter_table <- parameter_table[parameter_order, ]
    
    dic_table <- sapply(dics, function(X) unclass(get_dic(X)))
    dic_table <- cbind(parameter=row.names(dic_table), dic_table)
    
    rbind(parameter_table, dic_table)
}
