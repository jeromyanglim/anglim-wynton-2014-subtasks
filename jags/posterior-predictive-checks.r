reshape_simple <- function(long_data, id, variable_name, response) {
    # long_data: data.frame that contains the three variables, id, variable_name, and response
    # id: name of the id variable in long_data (e.g., "id")
    # variable_name: name of variable in long_data that represents the variable when in wide format
    # response: the name of the 
    # return: returns a data frame in wide format
    tempRawData <- long_data[, c(id, variable_name, response)]
    names(tempRawData) <- c(id, variable_name, 'response') 
    tempWideData <- reshape(tempRawData, idvar=id, direction='wide', timevar=variable_name)  
    names(tempWideData) <-  sub('response.', '', names(tempWideData))
    tempWideData
}

sd_ratio_15to1 <- function(x) {
    ratio_15to1 <- function(x, subtask) {
        y15 <- x[x$trial == 15 & x$subtask == subtask, 'y']
        y1 <- x[x$trial == 1 & x$subtask == subtask, 'y']  
        y1 <- ifelse(y1 == 0, .1, y1)
        y15 <- ifelse(y15 > y1, y1, y15)
        y15/y1
    }
    ratios <- sapply(1:3, function(X) ratio_15to1(x, X))
    apply(ratios, 1, sd)
}

average_subtask_correlation <- function(x) {
    per_person <- function(x)  {
        x$y <- x$y + rnorm(x$y, 0, .0001)
        rx <- reshape_simple(x[, c('trial', 'subtask', 'y')], id='trial', variable_name='subtask', response='y')
        corx <- cor(rx[2:4])
        mean(corx[lower.tri(corx)])
    }
    sx <- split(x, x$subject)
    sapply(sx, per_person)
}


calculate_statistics <- function(simulations, dataset=Data) {
    # TODO: update to ensure that it is robust to sort order of data.frame
    stats <- function (x) {
        # x: single dataset
        list(y1minus15_i_mean = mean(x[x$trial == 1 & x$subtask == 1, 'y'] - x[x$trial == 15 & x$subtask == 1, 'y']),
             y1minus15_f_mean = mean(x[x$trial == 1 & x$subtask == 2, 'y'] - x[x$trial == 15 & x$subtask == 2, 'y']),
             y1minus15_t_mean = mean(x[x$trial == 1 & x$subtask == 3, 'y'] - x[x$trial == 15 & x$subtask == 3, 'y']),
             y1minus15_i_sd = sd(x[x$trial == 1 & x$subtask == 1, 'y'] - x[x$trial == 15 & x$subtask == 1, 'y']),
             y1minus15_f_sd = sd(x[x$trial == 1 & x$subtask == 2, 'y'] - x[x$trial == 15 & x$subtask == 2, 'y']),
             y1minus15_t_sd = sd(x[x$trial == 1 & x$subtask == 3, 'y'] - x[x$trial == 15 & x$subtask == 3, 'y']),
             sd_ratio_15to1_mean = mean(sd_ratio_15to1(x)),
             sd_ratio_15to1_sd = sd(sd_ratio_15to1(x)),
             average_subtask_correlation_mean = mean(average_subtask_correlation(x)),
             average_subtask_correlation_sd = sd(average_subtask_correlation(x))
        )
    }
    simulations_stats <- sapply(simulations, stats)
    dataset_stats <- stats(Data)
    list(simulations=simulations_stats, dataset=dataset_stats)
}