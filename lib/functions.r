subtask_consistency_loess <- function(dv, iv, subtask, data) {
    x <- data[,c(iv, dv, subtask)]
    xs <- split(x[,c(iv,dv)], f=x[,subtask])
    
    xsp <- lapply(xs, function(X) 
        predict(loess(as.formula(paste(dv, "~", iv)), data=X)) )
    
    mean(    cor(xsp[[1]], xsp[[2]]),
             cor(xsp[[2]], xsp[[3]]),
             cor(xsp[[1]], xsp[[3]])
    )
    
}

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
