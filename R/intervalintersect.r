

#' Intersect intervals within groups
#'
#' Given two tables each containing a set of intervals, find all interval intersections within groups.  Returns
#' a data.table containing all columns from both tables.  One use of this function is to take a table
#' containing an address history (a table containing the intervals when study participants lived at past addressess)
#' and join it to an exposure history table (a complete set of exposure predictions for each address,
#' where the exposures are stored as the average value over a set of intervals) returning the set of
#' exposure intervals at addresses clipped to exactly when the participant lived at that address.
#'
#'
#'Technically speaking this is just an inner cartesian join where the last two join variables are
#' doing a non-equi join for partial overlaps. Then each interval intersect is calculated using max and min.
#'
#' If there are columns with the same names in both x and y (including interval_vars
#' but excepting group_vars), the return value will still return both columns. The column in y
#' will be names as it was originally and the column in x will be prepended with the letter i followed with a dot:
#' \code{i.} \cr
#'
#' Note that the function returns the same result if you switch x and y
#'  (with the exception of switched column names in the case of column name conflicts as just discussed)
#' this function is basically just a wrapper for the following code:\cr \cr
#'
#' \code{data.table::setkeyv(x, c(group_vars,interval_vars))} \cr
#' \code{data.table::setkeyv(y, c(group_vars,interval_vars))} \cr
#' \code{#do a cartesian inner interval join,} \cr
#' \code{#within groups defined by the interaction of the group_vars variables:}\cr
#' \code{z <- data.table::foverlaps(x,y,nomatch=NULL)} \cr
#' \code{# then, in z, for each row, return the row-wise maximum of} \cr
#' \code{#the interval starts from x and y and return the row-wise mininum} \cr
#' \code{#of the interval ends from x and y:}\cr
#' \code{<see source code>}
#'
#'
#'
#' @param x A data.table of exposures with two columns designating intervals with a column for address id and a column for ppt it.
#' @param y A data.table containing an address history (with two columns designating intervals)
#'  where these intervals are non-overlapping within ppt_id
#' @param interval_vars Either a length-2 character vector denoting column names in both x and y or a named
#'  length-2 character vector where the names are column names in x and the values are column names in y.
#' @param group_vars NULL, or either a character vector denoting the column name(s) in x and y,
#'  or a named character vector where the name is the column name in x and the value is the column name in y.
#'  This/these column(s) serve as an additional keying variable in the join (ie in addition to the interval join)
#'  such that intervals in x will only be joined to overlapping in intervals in y when the group_vars values are the same.
#' @param interval_vars_out The column names of the interval columns in the return data.table.
#' By default the return table will contain columns \code{c("start","end")}.
#' If your input tables already contain these columns,
#' you need to either specify \code{interval_vars_out} to be non-conflicting names with columns in x and y.
#' Or you rename columns in x and y to not contain columns named \code{c("start","end")}.
#' @return A data.table with columns \code{interval_vars_out} which denote the start and
#' stop period for each interval. This return table also contains columns in x and y. See details
#' for how naming conflicts are dealt with.
#' @seealso \code{\link{is.overlapping}} To test if a table contains overlapping intervals within values of \code{group_vars}
#' @examples
#'
#'y <- data.table(addr_id=c(1,2,2,3,5),
#'ppt_id=c(1,1,1,2,2),
#'addr_start=c(1,10,12,1,1),
#'addr_end=c(9,11,14,17,10))
#'x <- data.table(addr_id=rep(1:4,each=3),
#'exposure_start=rep(c(1,8,15),times=4),
#'exposure_end=rep(c(7,14,21),times=4),
#'exposure_value=c(rnorm(12))
#')
#'intervalintersect(x,y,
#'interval_vars=c(exposure_start="addr_start",exposure_end="addr_end"),
#'"addr_id")
#'y2 <- data.table(addr_id=c(1,2,2,2,3),
#'ppt_id=c(1,1,1,1,2),
#'addr_start=c(1,2,3,4,1),
#'addr_end=c(9,12,13,8,10))
#'
#'#intervalintersect will still work when there are overlapping intervals within a table:
#'is.overlapping(y2,interval_vars =c("addr_start","addr_end") ,group_vars="addr_id")
#'
#'intervalintersect(x,y2,
#'                  interval_vars=c(exposure_start="addr_start",exposure_end="addr_end"),
#'"addr_id")
#'
#'
#'x2 <- data.table(addr_id=rep(1:4,each=3),
#'exposure_start=rep(c(1,7,14),times=4),
#'                 exposure_end=rep(c(7,14,21),times=4),
#'exposure_value=c(rnorm(12))
#')
#'is.overlapping(x2,interval_vars =c("exposure_start","exposure_end") ,group_vars="addr_id")
#'
#'intervalintersect(x2,y2,
#'interval_vars=c(exposure_start="addr_start",exposure_end="addr_end"),
#'"addr_id")
#' #however, it may be meaningful isolate intervals of partial-overlap within
#' #each table and deal with them
#' #prior to intersecting the tables together:
#'
#'x2z <- isolateoverlaps(x2,interval_vars=c("exposure_start","exposure_end"),group_vars=c("addr_id"),
#'interval_vars_out=c("exposure_start2","exposure_end2"))
# x2b represents x2 when where exposure values in overlapping intervals have been averaged
#'x2b <- x2z[, list(exposure_value=mean(exposure_value)),
#'  by=c("addr_id","exposure_start2","exposure_end2")]
#'data.table::setnames(x2b, c("exposure_start2","exposure_end2"),c("exposure_start","exposure_end"))
#'
#'y2z <- isolateoverlaps(y2,interval_vars=c("addr_start","addr_end"),group_vars=c("addr_id"),
#'interval_vars_out = c("addr_start2","addr_end2"))
#'y2b <- unique(y2z[, list(addr_id, ppt_id,addr_start2,addr_end2)])
#'data.table::setnames(y2b, c("addr_start2","addr_end2"), c("addr_start","addr_end"))
#y2b represents where address history which are overlapping in the same address are deduped:
#'
#'intervalintersect(x2b,y2b,
#'interval_vars=c(exposure_start="addr_start",exposure_end="addr_end"),
#'"addr_id")
#'
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

  data.table::setkeyv(y,c(y_group_vars,y_interval_vars ))
  data.table::setkeyv(x,c(x_group_vars,x_interval_vars ))

  z <- data.table::foverlaps(x,y,nomatch=NULL)

  #column name conflicts in x and y in foverlaps are resolved by prepending "i." to columns from x
  if(is.null(names(interval_vars))){
    x_interval_vars <- paste0("i.",interval_vars)
  }

  #take the later of the two start dates and the earlier of the two end dates
  data.table::set(z, j=interval_vars_out[1],value=pmax(z[[x_interval_vars[1]]],z[[y_interval_vars[1]]] ) )
  data.table::set(z, j=interval_vars_out[2],value=pmin(z[[x_interval_vars[2]]],z[[y_interval_vars[2]]] ) )
  z[]
}
