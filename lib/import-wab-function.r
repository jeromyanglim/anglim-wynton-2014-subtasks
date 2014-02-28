import_wab_data <- function(data, outliertrial=100, blocktotal = 15) {
    # outliertrial: number in seconds
    # blocktotal: number of blocks that trials should be integrated into
    # import data
    rita <- data
    
    names(rita) <- tolower(names(rita))
    rita$gap <- rita$gap / 1000
    rita$trial_time<- rita$trial_time / 1000
    rita$task_time <- rita$task_time / 1000
    strings_to_clean <- c("activity", "subtask", "answer_age", "answer_level", "answer_teacher",  "answer_days", "answer_times" )
    rita[, strings_to_clean] <- sapply(rita[,strings_to_clean], function(X) gsub(" ", "", tolower(X)))
    rita$user_id <- toupper(rita$user_id)

    # temp <- paste(rita$user_id, rita$trial_no, rita$action_no)
    # sum(duplicated(temp))
    
    # create raw trial time file
    rit <- aggregate(rita$gap,  list(rita$user_id, rita$trial_no), sum)
    names(rit) <- c('user_id', 'trial_no', 'rt')

    # removal of trials
    # remove trial 1 and trials 
    rit <- rit[rit$trial != 1 , ]
    
    # plot(density(rit$rt), xlim=c(0, 120))
    # plot(density( rit[ rit$trial_no < 10,  "rt"] ), xlim=c(0, 140))
    # xyplot(rt ~ trial_no | user_id, rit, ylim=c(0, 80))
    
    rrit <- rit
    rit <- rit[rit$rt < outliertrial, ]
    
    # plot(density(table(rit$user_id)))
    
    # reinitialise the trials
    rit <- rit[order(rit$user_id, rit$trial_no), ]
    rit$trial <- unsplit(sapply(split(rit$trial_no, rit$user_id), order), rit$user_id)
    

    # create raw subtask trial time
    rits <- aggregate(rita$gap,  list(rita$user_id, rita$trial_no, rita$subtask), sum)
    names(rits) <- c('user_id', 'trial_no', 'subtask', 'rt')
    # xyplot(rt ~ trial | user_id, groups=subtask, rits, ylim=c(0, 20))
    
    # block trials
    tot_trials <- data.frame(table(rit$user_id))
    names(tot_trials) <- c('user_id', 'trial_count')
    rit <- merge(rit, tot_trials, all.x=TRUE)
    rit$block <- ceiling(blocktotal * rit$trial / rit$trial_count)
    # xyplot(rt ~ block | user_id, rit, ylim=c(0, 60))
    
    
    # aggregate to blocks
    rib <- aggregate(rit$rt,  list(rit$user_id, rit$block), mean)
    names(rib) <- c('user_id', 'block', 'rt')

    # incorporat blocks into subtasks and deleted trials
    rits <- merge(rits, rit[, c('user_id', 'trial_no', 'trial', 'block')]) 

    ribs <- aggregate(rits$rt,  list(rits$user_id, rits$subtask, rits$block), mean)
    names(ribs) <- c('user_id', 'subtask', 'block', 'rt')
    # xyplot(rt ~ block | user_id, group=subtask, ribs, ylim=c(0, 20), type='l')

    ################# STRATEGIES #####################
    # level filter
    # time filter
    # time question
    # each filter
    # deselect all on level filter
    # button pressing (accuracy or speed)
    # number of questions asked
    # number of irrelevant questions asked
    # access rules
    # reading speed (time spend reading a question; e.g., age) 

    activity_present <- function(activities, strategy_name) {
        rita[,strategy_name] <- rita$activity %in% activities
        temp <- aggregate(rita[, strategy_name],  list(rita$user_id, rita$trial_no), 
                          function(X) as.numeric(sum(X) > 0))
        names(temp) <- c('user_id', 'trial_no', strategy_name)
        merge(rit, temp)
    }
    
    # filters
    rit <- activity_present( grep("times", unique(rita[ rita$subtask == 'filtering', 'activity']), value=TRUE), 'strategy_filter_time')
    rit <- activity_present(grep("level", unique(rita[ rita$subtask == 'filtering', 'activity']), value=TRUE), 'strategy_filter_level')
    rit <- activity_present(grep("day", unique(rita[ rita$subtask == 'filtering', 'activity']), value=TRUE) , 'strategy_filter_day')
    rit <- activity_present(grep("teacher", unique(rita[ rita$subtask == 'filtering', 'activity']), value=TRUE) , 'strategy_filter_teacher')
    rit <- activity_present(grep("class", unique(rita[ rita$subtask == 'filtering', 'activity']), value=TRUE) , 'strategy_filter_classsize')
    
    # time question
    rit <- activity_present("askedfortimes",  'strategy_question_time')
    rit <- activity_present("askedforname",  'strategy_question_name')
    rit <- activity_present("askedforexperience",  'strategy_question_experience')
    rit <- activity_present("askedforlevel",  'strategy_question_level')
    rit <- activity_present("askedforteacher",  'strategy_question_teacher')
    rit <- activity_present("askedforspecial_needs",  'strategy_question_specialneeds')
    rit <- activity_present("askedforage",  'strategy_question_age')
    
    # access rules
    rit <- activity_present("askedforclassdetails",  'strategy_accessrules')
    
    # deselect all on level filter
    rit <- activity_present("setalllevelsoff",  'strategy_filter_level_off')
    
    # number of unique questions
    rit$strategy_questions_all_unique_sum <- apply(rit[,c("strategy_question_time", "strategy_question_name", "strategy_question_experience", "strategy_question_level", 
                                                          "strategy_question_teacher", "strategy_question_specialneeds",  "strategy_question_age")], 1, sum)
    
    # number of irrelevant questions asked
    rit$strategy_questions_irrelevant_unique_sum <- apply(rit[,c("strategy_question_name", "strategy_question_experience", 
                                                                 "strategy_question_specialneeds")], 1, sum)
    
    rit$strategy_questions_irrelevant_unique_mean <- rit$strategy_questions_irrelevant_unique_sum / 3
    
    
    # RT for first actions
    rt_activity_first <- function(activity, variable_name) { 
        rita <- rita[order(rita$user_id, rita$trial_no, rita$action_no), ]
        temp <- rita[rita$activity == activity,  c('user_id', 'trial_no','action_no', 'gap')]
        names(temp) <- c('user_id', 'trial_no','action_no', variable_name)
        temp$user_id_trial_no <- paste(temp$user_id, temp$trial_no)
        temp <- temp[!duplicated(temp$user_id_trial_no), ]
        merge(rit, temp[,c('user_id', 'trial_no', variable_name)])
    }
    
    # button pressing (accuracy or speed)
    rit <- rt_activity_first("askedforage", 'rt_askforage')
    
    # reading speed
    rit <- rt_activity_first("okedagequestion", 'rt_okedagequestion')
    rit <- rt_activity_first("okedtimesquestion", 'rt_okedtimesquestion')
    
    # sort(grep("", unique(rita[ rita$subtask == 'informationgathering', 'activity']), value=TRUE))
    # number of times questions were asked
    
    
    # aggregate strategies to block
    aggregation_variables <- c( grep("strategy_", names(rit), value=TRUE),
                                grep("rt_", names(rit), value=TRUE))
    temp <- aggregate(rit[,aggregation_variables], list(user_id=rit$user_id, block=rit$block), mean)
    rib <- merge(rib, temp)
    
    # plot of block by strategy
    # for(i in seq(3, length(names(rib)))) {
    #     print(xyplot(formula(paste(names(rib)[i]," ~ block | user_id")), rib, main=names(rib)[i]))
    # }
    
    
    # extract trial properties
    rit <- merge(rit,  rita[ rita$action_no == 1, c("user_id", 'trial_no', "answer_age", "answer_level", "answer_teacher",  "answer_days", "answer_times", "classes_possible")])
    
    # create 
    rb_agg_var <- setdiff(names(rib), c('user_id', 'block'))
    rb <- aggregate(rib[,rb_agg_var], list(block=rib$block), mean)
    
    rbs_agg_var <- setdiff(names(ribs), c('user_id', 'subtask', 'block'))
    rbs <- aggregate(ribs[,rbs_agg_var, drop=FALSE], list(block=ribs$block, subtask=ribs$subtask), mean)

    # make non existent subtasks 0
    all_ribs <- expand.grid(user_id=unique(ribs$user_id), block=unique(ribs$block), subtask=unique(ribs$subtask))
    ribs <- merge(ribs, all_ribs, all.x=TRUE, all.y=TRUE)
    ribs$rt[is.na(ribs$rt)] <- 0
    # merge rib into ribs
    ribs <- merge(ribs, subset(rib, select = -rt))
    
    list(rita=rita, rit=rit, rib=rib, rits=rits, ribs=ribs, rb=rb, rbs=rbs, rrit =rrit)

}