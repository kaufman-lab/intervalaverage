

#' Clip exposures to a non-overlapping address history
#'
#' Given an address history where each participant has possibly multiple addresses that are non-overlapping in time,
#' join this address history to an exposure table that has a complete set of exposures
#' (ie, the same period for every address) for all addresses such that the resulting table has exposures
#' only in intervals when that address was lived at. Note that this function does not do any averaging,
#' it just clips the exposures to the address history.
#'
#'
#'
#'
#' @param x A data.table of exposures with two columns designating intervals with a column for address id and a column for ppt it.
#' @param y A data.table containing an address history (with two columns designating intervals)
#'  where these intervals are non-overlapping within ppt_id
#' @param interval_vars Either a length-2 character vector denoting columns in both x and y or a named
#'  length-2 character vector where the names are columns in x and the values are columns in y.
#' @param addr_id Either a length-1 character vector denoting the column name in x and y,
#'  or a length-1 named character vector where the name is the column name in x and the value is the column name in y.
#' @param ppt_id Either a length-1 character vector denoting the column name in y
#' @param interval_vars_out The column names of the interval columns in the return data.table.
#' By default the return table will contain columns \code{c("start","end")}.
#' If your input tables already contain these columns,
#' you need to alter \code{interval_vars_out} to select non-conflicting names.
#'  These need to be chosen to be non-conflicting with other columns in x and y.
#' @return A data.table with columns \code{interval_vars_out} which denote the start and
#' stop period for each interval. This return table also contains columns in x and y.
#'
#'
#' @examples
#'
#' x <- data.table(addr_id=c(1,1,2,2,3,3,4,4),
#'  exposure_start=c(1,6,1,6,1,6,1,6),
#'  exposure_end=c(5,10,5,10,5,10,5,10)
#' )
#' y <- data.table(addr_id=c(1,2,2,3),ppt_id=c(1,1,1,2),addr_start=c(1,8,10,1), addr_end=c(7,9,10,15))
#' clip_exposures(x,y,interval_vars=c(exposure_start="addr_start",exposure_end="addr_end"),"addr_id","ppt_id")
#' @export
clip_exposures <- function(x,y, interval_vars, addr_id,ppt_id,
                           interval_vars_out=c("start","end")
                           ){

  if( any(interval_vars_out %in% names(x))|any(interval_vars_out %in% names(y)) ){
    stop("interval_vars_out cannot be names in x or y. choose a different output name")
  }

  x_interval_vars <- if(!is.null(names(interval_vars))){names(interval_vars)}else{interval_vars}
  y_interval_vars <- interval_vars

  x_addr_id <- if(!is.null(names(addr_id))){names(addr_id)}else{addr_id}
  y_addr_id <- addr_id

  ##address table must be non-overlapping
  stopifnot(
    !intervalaverage::is_overlapping(y,
                                     interval_vars=y_interval_vars,
                                     group_vars=ppt_id
    )
  )

  u_x_add <- unique(x[[x_addr_id]])
  u_y_add <- unique(y[[y_addr_id]])

  sdiffyx <- setdiff(u_y_add,u_x_add)
  sdiffxy <- setdiff(u_x_add,u_y_add)

  if(length(sdiffyx)){
    warning(paste("There are",length(sdiffyx),"addresses in address history table (y) but not the exposure table (x)"))
  }
  if(length(sdiffxy)){
    warning(paste("There are",length(sdiffxy),"addresses in the exposure table (x) but not the address history table (y)"))
  }

  setkeyv(y,c(y_addr_id,y_interval_vars ))
  setkeyv(x,c(x_addr_id,x_interval_vars ))

  z <- foverlaps(y,x,nomatch=NULL)

  #take the later of the two start dates and the earlier of the two end dates
  set(z, j=interval_vars_out[1],value=pmax(z[[x_interval_vars[1]]],z[[y_interval_vars[1]]] ) )
  set(z, j=interval_vars_out[2],value=pmin(z[[x_interval_vars[2]]],z[[y_interval_vars[2]]] ) )
  z[]
}
