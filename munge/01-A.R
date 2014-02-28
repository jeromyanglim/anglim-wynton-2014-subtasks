# apply main preprocessing script.
rawrita[ rawrita$User_Id == "Q RT 7", 'User_Id'] <- 'QRT753'
x <- import_wab_data(rawrita)
list2env(x,globalenv())

# rcases
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
ri <- as.data.frame(sapply(rcases, function(X) trim(X)))
names(ri) <- tolower(names(ri))


# variables
v <- list()
v$strategies <- grep("^strategy_", names(rib), value=TRUE)


lapply(rcases, table)
ri <- data.frame(user_id=unique(rib$user_id))

names(rcases) <- tolower(names(rcases))

