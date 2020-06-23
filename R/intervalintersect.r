

#' Intersect intervals within groups
#'
#' Given two tables each containing a set of intervals, find all interval intersections within groups.  Returns
#' a data.table containing all columns from both tables. One use of this function is to take a table
#' containing an address history (a table containing the intervals when study participants lived at past addressess)
#' and join it to an exposure history table (a complete set of exposure predictions for each address,
#' where the exposures are stored as the average value over a set of intervals) returning the set of
#' exposure intervals at addresses clipped to exactly when the participant lived at that address.
#'
#'
#' If there are columns with the same names in both x and y (including interval_vars
#' but excepting group_vars), the return value will still return both columns. The column in y
#' will be names as it was originally and the column in x will be prepended with the letter i followed with a dot:
#' \code{i.} \cr
#'
#' Note that the function returns the same result if you switch x and y
#'  (with the exception of switched column names in the case of column name conflicts as just discussed)
#' this function is basically just a wrapper for the following code:\cr \cr
#' \code{
#' setkeyv(x, c(group_vars,interval_vars)) \cr
#' setkeyv(y, c(group_vars,interval_vars)) \cr
#' #do a cartesian inner interval join,
#' \cr
#'   #within groups defined by the interaction of the group_vars variables:
#'   \cr
#' z <- foverlaps(x,y,nomatch=NULL) \cr
#' # then, in z, for each row, return the row-wise maximum of
#' \cr
#' #the interval starts from x and y and return the row-wise mininum
#' \cr
#'  #of the interval ends from x and y:
#'  \cr
#'  <see source code>
#' }
#'
#'
#' @param x A data.table of exposures with two columns designating intervals with a column for address id and a column for ppt it.
#' @param y A data.table containing an address history (with two columns designating intervals)
#'  where these intervals are non-overlapping within ppt_id
#' @param interval_vars Either a length-2 character vector denoting columns in both x and y or a named
#'  length-2 character vector where the names are columns in x and the values are columns in y.
#' @param group_vars Either a length-1 character vector denoting the column name in x and y,
#'  or a length-1 named character vector where the name is the column name in x and the value is the column name in y.
#' @param interval_vars_out The column names of the interval columns in the return data.table.
#' By default the return table will contain columns \code{c("start","end")}.
#' If your input tables already contain these columns,
#' you need to alter \code{interval_vars_out} to select non-conflicting names.
#'  These need to be chosen to be non-conflicting with other columns in x and y.
#' @return A data.table with columns \code{interval_vars_out} which denote the start and
#' stop period for each interval. This return table also contains columns in x and y. See details
#' for how naming conflicts are dealt with.
#'
#'
#' @examples
#'
#'  set.seed(32)
#'x <- data.table(addr_id=rep(1:4,each=2),
#'  exposure_start=rep(c(1,8),times=4),
#'  exposure_end=rep(c(7,14),times=4),
#'  exposure_value=c(rnorm(8))
#')
#'y <- data.table(addr_id=c(1,2,2,3),
#'ppt_id=c(1,1,1,2),
#'addr_start=c(1,10,12,1),
#'addr_end=c(9,11,14,17))
#'intervalintersect(x,y,
#'interval_vars=c(exposure_start="addr_start",exposure_end="addr_end"),
#' @export
intervalintersect <- function(x,y, interval_vars, group_vars=NULL,
                           interval_vars_out=c("start","end")
                           ){

  if( any(interval_vars_out %in% names(x))|any(interval_vars_out %in% names(y)) ){
    stop("interval_vars_out cannot be names in x or y. choose a different output name")
  }

  x_interval_vars <- if(!is.null(names(interval_vars))){names(interval_vars)}else{interval_vars}
  y_interval_vars <- interval_vars

  x_group_vars <- if(!is.null(names(group_vars))){names(group_vars)}else{group_vars}
  y_group_vars <- group_vars

  setkeyv(y,c(y_group_vars,y_interval_vars ))
  setkeyv(x,c(x_group_vars,x_interval_vars ))

  z <- foverlaps(x,y,nomatch=NULL)

  #column name conflicts in x and y in foverlaps are resolved by prepending "i." to columns from x
  if(is.null(names(interval_vars))){
    x_interval_vars <- paste0("i.",interval_vars)
  }

  #take the later of the two start dates and the earlier of the two end dates
  set(z, j=interval_vars_out[1],value=pmax(z[[x_interval_vars[1]]],z[[y_interval_vars[1]]] ) )
  set(z, j=interval_vars_out[2],value=pmin(z[[x_interval_vars[2]]],z[[y_interval_vars[2]]] ) )
  z[]
}
