check_list <- function(x,
                       ...,
                       allow_na = FALSE,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!missing(x) && (is.list(x) || (allow_null && is.null(x)))) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    c("a list"),
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_class <- function(x, cls, ..., allow_null = FALSE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  what <- paste0("a <", cls, "> object or `NULL`")

  if (allow_null) {
    if (!is.null(x) && !inherits_any(x, cls)) {
      stop_input_type(x, what, ..., arg = arg, allow_null = allow_null, call = call)
    }
  } else {
    if (!inherits_any(x, cls)) {
      stop_input_type(x, what, ..., allow_null = allow_null, arg = arg, call = call)
    }
  }
}

check_char <- function(x,
                       ...,
                       allow_na = FALSE,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  check_string(x, ..., allow_na = allow_na, allow_null = allow_null, arg = arg, call = call)

  if (nchar(x) == 1) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a single character",
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
