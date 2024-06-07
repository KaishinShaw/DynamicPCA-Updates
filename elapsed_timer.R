create_timer <- function() {
    start_time <- Sys.time()

    list(
        start = function() {
            start_time <<- Sys.time()
        },
        stop = function() {
            end_time <- Sys.time()
            elapsed_time <- end_time - start_time
            return(elapsed_time)
        }
    )
}
