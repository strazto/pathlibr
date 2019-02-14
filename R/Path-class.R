
#' OO Path Navigator
#'
#' @concept class
#' @family Path
#' @importFrom rlang %||%
#' @importFrom magrittr %<>%
#' @export
Path <- R6::R6Class(
  "Path",
  public = list(
    initialize = function(path = NULL) private$abstract,
    join = function(other) private$abstact,
    climb = function(levels) private$abstract,
    print = function() print(self$show)
  ),
  active = list(
    # Active Bindings =======
    name = function(x) private$abstract(),
    show = function(x) private$abstract(),
    dir = function(x) private$abstract(),
    . = function(x) self$dir,
    parent = function(x) private$abstract(),
    .. = function(x) self$parent
  ),
  private = list(
    # Private Variables =====

    path = NULL,
    get_children = function() private$abstract(),
    abstract = function() stop("Not written")
  )
)


# Implementations :::::::::::::========================

# Public :::::::::::::::::::::========

# intitialize ====

#' Initialize the Path object
#'
#' Initialize a Path object using a string
#'
#' @name Path$new
#' @examples
#' Path$new("path/components/as/string")
#' @family Path
#' @param path the path string to Path should refer to
#' @return a new Path object, corresponding to the supplied string
NULL
Path$set(
  "public", "initialize",
  function(path = NULL){
    if (is.null(path)) stop("Can't supply null path")
    private$path <- path
  },
  overwrite = TRUE
)

# join ====
#' Join two path components
#'
#' Given two path components, join them and return a new Path object
#'
#' @param other the path component to join on the RHS. Can be any object
#'        coercible to character, including `Path`
#' @examples
#' {
#' path <- Path$new("some/path")
#' path$join("other/path")
#' }
#' @name Path$join
#' @family Path
#' @return A new `Path` object resulting from the joined paths
NULL
Path$set(
  "public", "join",
  function(other){
    other %<>% as.character() %||% ""
    lhs <- self$show %||% ""

    lhs %>%
      file.path(other) %>%
      Path$new() %>%
    return()
  },
  overwrite = TRUE
)

# Active :::::::::::::::::::: ============

# dir ======

#' @inherit Path$dir
#' @name Path$.
#' @family Path
NULL
#' Get the elements in the directory
#'
#' Returns a named list of Path elements in a directory
#' @name Path$dir
#' @family Path
NULL
Path$set(
  "active", "dir",
  function(x){
    if (!missing(x)) stop("Dont assign to me!")
    out <- private$get_children() %>%
      purrr::map(Path$new)

    return(out)
  },
  overwrite = TRUE
)

# parent =====

#' @name Path$..
#' @family Path
#' @inherit Path$parent
NULL
#' Get the parent of the current Path
#'
#' @name Path$parent
#' @examples
#' path <- Path$new("root/parent/path")
#' print(glue::glue("path   : {path$show}",
#'                  "parent : {path$parent$show}",
#'                  "..     : {path$..$show}",
#'                  .sep = "\n"))
#' @family Path
#' @return a new Path object, corresponding to the parent of the calling Path
NULL
Path$set(
  "active", "parent",
  function(x){
    if (!missing(x)) stop("Don't assign to me!")

    out <- private$path %>% dirname() %>% Path$new()
    return(out)
  },
  overwrite = TRUE
)
# name =======

#' Get the name of the last element in the path (Incl. extension)
#'
#' @name Path$name
#' @family Path
#' @return the string corresponding to the path's name
#' @examples
#' path <- Path$new("path/to/some/element")
#' path$name
NULL
Path$set(
  "active", "name",
  function(x){
    if (!missing(x)) stop("Don't assign to me!")
    out <- private$path %>%
      pathr::parse_path() %>% pathr::back()
    return(out)
  },
  overwrite = TRUE
)

# show =======

#' Show the entire path as a string
#'
#' Returns the path as a string
#'
#' @name Path$show
#' @return the path as as tring
#' @family Path
NULL
Path$set(
  "active", "show",
  function(x){
    if (!missing(x)) stop("Don't assign to me!")
    out <- private$path
    return(out)
  },
  overwrite = TRUE
)

# Private :::::::::::::::::::=============================

# Get Children =======

#' Get the paths of the children of a directory
#'
#' @name Path$private$get_children
#' @family Path$private
#' @keywords internal
#' @return a named list, of `names : paths` for the children in the dir
#'         as strings
NULL
Path$set(
  "private", "get_children",
  function(){
    in_dir <- base::dir(private$path)

    paths <- pathr::file_path(self$show, in_dir)
    out <- list()
    out[in_dir] <- paths
    return(out)
  },
  overwrite = TRUE
)



# S3 Methods :::::::::::::========================

#' Implementation of S3 generic for as.character
#'
#' @export
#' @concept S3Methods
#' @keywords internal
#' @param x the Path object to be coerced to character
as.character.Path <- function(x){
  return(x$show)
}

#' Join two paths
#'
#' Join two path components in the "natural" way, as you would expect to
#'     using a `/` operator. `lhs` and `rhs` may be any object that implements
#'     the `as.character` S3 method, including `Path`
#' @usage lhs \%//\% rhs
#' @param lhs the path component(s) to be joined on lhs
#' @param rhs path components(s) to be joined on rhs
#' @return a new `Path` object (or vector of) corresponding to the joined
#'         elements
#' @family Path
#' @examples
#' path <- Path$new("root/parent/name")
#' child <- path %//% "child"
#' print(glue::glue("path : {path$show}",
#'                  "child: {child$show}",
#'                  .sep = "\n"))
#' @export
`%//%` <- function(lhs, rhs){
  lhs %<>% as.character() %>% Path$new()
  rhs %<>% as.character() %>% Path$new()
  return(lhs$join(rhs))
}
