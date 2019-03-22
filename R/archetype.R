# This is one possible implementation of the idea of archetypes.
# In this case, the user buils an archetype as if it were a named list.
# The archetype object is actually a function that returns the full request
# when evaluated.

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
# Main function. Takes a number of named arguments and returns a function
# that evaluates them. It automatically detects the arguments the function
# needs to take
#
wf_archetype <- function(query, ...) {
  query_exp <- rlang::enexpr(query)
  extra_args <- match.call(expand.dots = FALSE)$`...`
  has_default <- names(extra_args) != ""

  vars <- unique(c(all.vars(query_exp),
                   names(extra_args[has_default]),
                   as.character(extra_args[!has_default])
  ))

  args <- setNames(rep(list(rlang::expr()),
                       length(vars)),
                   vars)
  args[vars %in% c(names(extra_args))] <- extra_args[has_default]

  f <- rlang::new_function(args, query_exp)
  f
}

if (FALSE) {
#
# Usage:
#

ERAI <- wf_archetype(
  list(class = "ei",
       dataset = "interim",
       expver = "1",
       levtype = "pl",
       stream = "moda",
       type = "an",
       format = "netcdf",
       date = wf_format_dates(date),
       grid = res/res,
       levelist = wf_slash_vector(levs),
       param = "155.128",
       target = "output"),
  res = 3,
  beta
)

# ERAI is now a function what takes arguments date, res and levs and
# returns a list with the above expressions evaluated in the context of
# those arguments.

ERAI("2010-01-01", 3, 200)

# Pros:
#    * It's a simple syntax with one user-facing function.
#    * It doesn't involve the somewhat obscure glue syntax.
#    * RStudio autocompletion informs the user of the arguments required to build
#      the request.
#
# Cons:
#    * The idea of a function that returns a function (a function factory) is
#      not always easy to explain to users.
#    * Needs to allow for preprocessing variables.

}
