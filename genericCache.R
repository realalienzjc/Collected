# https://class.coursera.org/rprog-003/forum/thread?thread_id=1411

# Return a memoized version of the function 'fun'. Results
# of calling the memoized function are cached and returned
# on the next execution with the same arguments.
#
# Args:
#     fun: The function to memoize
#
# Returns:
#     A function that wraps 'fun' and caches the results
#
# Example:
#    memoized_solve <- memoize(solve)
memoize <- function(fun) {
    if (!is.function(fun)) {
        stop("fun is not a function type")
    }

    fun_ <- fun
    args_ <- NULL
    cache_ <- NULL
    memoized <- function(...) {
        new_args <- list(...)
        if (!identical(new_args, args_) ||
                identical(cache_, NULL)) {
            result <- fun_(...)
            args_ <<- new_args
            cache_ <<- result
        }
        cache_
    }
}

