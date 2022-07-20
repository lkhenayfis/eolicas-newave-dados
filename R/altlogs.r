############################################ DUMMY LOGS ############################################

func_logopen <- function(dolog) {
    if(dolog) return(logr::log_open) else return(function(...) invisible(NULL))
}

func_logprint <- function(dolog) {
    if(dolog) return(logr::log_print) else return(print)
}

func_logclose <- function(dolog) {
    if(dolog) return(logr::log_close) else return(function(...) invisible(NULL))
}
