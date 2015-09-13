#' Merge two lists and overwrite latter entries with former entries
#' if names are the same.
#'
#' For example, \code{list_merge(list(a = 1, b = 2), list(b = 3, c = 4))}
#' will be \code{list(a = 1, b = 3, c = 4)}.
#' @param list1 list
#' @param list2 list
#' @return the merged list.
#' @export
#' @examples
#' stopifnot(identical(list_merge(list(a = 1, b = 2), list(b = 3, c = 4)),
#'                     list(a = 1, b = 3, c = 4)))
#' stopifnot(identical(list_merge(NULL, list(a = 1)), list(a = 1)))
list_merge <- function(list1, list2) {
  list1 <- list1 %||% list()
  # Pre-allocate memory to make this slightly faster.
  list1[Filter(function(x) nchar(x) > 0, names(list2) %||% c())] <- NULL
  for (i in seq_along(list2)) {
    name <- names(list2)[i]
    if (!identical(name, NULL) && !identical(name, "")) list1[[name]] <- list2[[i]]
    else list1 <- append(list1, list(list2[[i]]))
  }
  list1
}

`%||%` <- function(x, y) if (is.null(x)) y else x

is.acceptable_function <- function(x) {
  is.function(x) || 
  is.null(x)     ||
  is.mungebit(x)
}

# If an environment contains variables "a" and "b",
# create a list (a = quote(a), b = quote(b)).
env2listcall <- function(env) {
  names <- ls(env)
  if ("name_order" %in% names(attributes(env))) {
    names <- names[attr(env, "name_order")]
  }
  setNames(lapply(names, as.name), nm = names)
}

make_env <- function(lst, parent = emptyenv()) {
  initial_names <- names(lst) %||% character(length(lst))
  names(lst) <- ifelse(unnamed(lst),
    paste0("_", seq_along(lst)),
    paste0("_", initial_names)
  )

  if (length(lst) == 0) {
    env <- new.env(parent = parent)
  } else {
    env <- list2env(lst, parent = parent)
  }

  name_order <- match(names(lst), ls(env))
  attr(env, "name_order")    <- name_order
  attr(env, "initial_names") <- initial_names
  env
}

unnamed <- function(el) {
  "" == (names(el) %||% character(length(el)))
}

unnamed_count <- function(el) {
  sum(unnamed(el))
}

