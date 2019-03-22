#' This is one possible implementation of the idea of archetypes.
#' In this case, the user buils an archetype as if it were a named list.
#' The archetype object is actually a function that returns the full request
#' when evaluated.

#
# Some helper functions to format dates and vectors into the MARS format
#
wf_format_dates <- function(dates) {
  paste0(lubridate::year(dates),
         formatC(lubridate::month(dates), width = 2, flag = "0"),
         formatC(lubridate::day(dates), width = 2, flag = "0"),
         collapse = "/")
}

wf_slash_vector <- function(vector) {
  paste0(vector, collapse = "/")
}


#
# Main function. Takes a list and optional arguments.
# Returns a function that takes arguments and return the populated request.
#
wf_archetype <- function(query, ...) {
  query_exp <- rlang::enexpr(query)
  extra_args <- match.call(expand.dots = FALSE)$`...`
  has_default <- names(extra_args) != ""

  vars <- unique(c(all.vars(query_exp),
                   names(extra_args[has_default]),
                   as.character(extra_args[!has_default])))

  args <- setNames(rep(list(rlang::expr()),
                       length(vars)),
                   vars)
  args[vars %in% c(names(extra_args))] <- extra_args[has_default]

  f <- rlang::new_function(args, query_exp)
  class(f) <- c("ecmwfr_archetype", class(f))
  f
}

# Functions for pretty printing
as.list.ecmwfr_archetype <- function(x, ...) {
  as.list(body(x))[-1]
}

print.ecmwfr_archetype <- function(x, ...) {
  components <- as.list(x)
  is_dynamic <- lapply(components, class) == "call"
  max_char_name <- max(vapply(names(components), nchar, 1))
  texts <- vapply(components, deparse, "a")
  max_char_text <- max(nchar(texts))

  rpad <- function(text, width) {
    formatC(text, width = -width, flag = " ")
  }

  cat("Request archetype with values: \n")
  for (comps in seq_along(components)) {
    star <- ifelse(is_dynamic[comps], " *", "")
    cat(" ",
        rpad(names(components)[comps], max_char_name),
        "=",
        rpad(texts[comps], max_char_text), star, "\n")
  }
  cat("arguments: ")
  args <- formals(x)
  for (a in seq_along(args)) {
    cat(names(args)[a])
    if (args[[a]] != rlang::expr()) {
      cat(" =", args[[a]])
    }
    if (a != length(args)) cat(", ", sep = "")
  }
}

#' Usage:
ERAI <- wf_archetype(
  list(class = "ei",
       dataset = "interim",
       expver = "1",
       levtype = "pl",
       stream = "moda",
       type = "an",
       format = "netcdf",
       date = wf_format_dates(date),
       grid = paste0(res, "/", res),
       levelist = wf_slash_vector(levs),
       param = "155.128",
       target = "output"),
  res = 3                               # sets default argument
)

#' ERAI is now a function what takes arguments date, res and levs and
#' returns a list with the above expressions evaluated in the context of
#' those arguments.

str(ERAI("2010-01-01", 3, 200))

#' And with a nice printing method.
print(ERAI)

#' Testing that it works
ecmwfr::wf_request(ERAI("2010-01-01", 3, 200),
                   "eliocampitelli@gmail.com")

#' Pros:
#'    * It's a simple syntax with one user-facing function.
#'    * It doesn't involve the somewhat obscure glue syntax.
#'    * RStudio autocompletion informs the user of the arguments required to build
#'      the request.
#'
#' Cons:
#'    * The idea of a function that returns a function (a function factory) is
#'      not always easy to explain to users.
#'    * Needs to allow for preprocessing variables.


