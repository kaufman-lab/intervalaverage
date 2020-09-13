#' Test for self-overlap in a data.table or a pair of vectors
#'
#' `is.overlapping` tests whether a data.table contains intervals which partially or completely overlap
#' with other intervals in different rows, possibly within groups. `is.overlappingv` tests whether
#'  intervals defined by a pair of vectors contains intervals which partially or completely overlap.
#'
#'
#' These functions provide a fast check to see if any intervals in a set overlap.
#' To actually identify specific segments of overlap,
#' the `data.table::foverlap` can be use to return self-join for partial and/or exactly overlapping
#' intervals.
#'
#' `intervalaverage::isolateoverlaps` can be used to break partially overlapping intervals
#' into separate intervals of exact overlap and non-overlap.
#'
#' Note that it's also possible to use `data.table::foverlap` directly to test the
#' existence of overlaps in a set of intervals
#' (in fact, that's a what `is.overlapping` did in version 0.8.0 and before).
#' However, this performs a join and if there are many overlaps then `foverlaps`
#' will join them all--this is unnecessarily slow and memory intensive if the goal
#' is to just test for the existence of one or more overlap.
#'
#' @rdname is.overlapping
#' @param x A data.table with two columns defining closed intervals (see also interval_vars).
#' @param interval_vars A length-2 character vector corresponding to column names of x which designate
#' the closed (inclusive) starting and ending intervals. The column name
#' specifying the lower-bound column must be specified first.
#' these columns be of the same class and either be integer or IDate.
#' @param group_vars NULL or a character vector corresponding to column names of x.
#' overlap checks will occur within groups defined by the columns specified here.
#' @param verbose prints additional information, default is FALSE
#' @return length-1 logical vector. TRUE if there are any overlaps, FALSE otherwise.
#' (note that a single pair of overlapping intervals within a single group specified by group_vars
#' will result in TRUE)
#'
#' @examples
#' x <- data.table(start=c(1L,2L),end=c(3L,4L))
#' is.overlapping(x,c("start","end")) #the interval 1,3 overlaps with the interval 2,4
#'

#' y <- data.table(start=c(1L,3L),end=c(2L,4L))
#' is.overlapping(y,c("start","end")) #the interval 1,2 doesn't overlap other intervals in y
#' z <- data.table(start=c(1L,3L,1L,2L),end=c(2L,4L,3L,4L),id=c(1,1,2,2))
#' is.overlapping(z,c("start","end"),"id")
#'
#' #note the above returns TRUE because there is an overlap in the group defined by z$id==2
#' #to identify which group(s) are overlapping:
#'
#'
#' @export
is.overlapping <- function(x,interval_vars,group_vars=NULL,verbose=FALSE){

  V1 <- NULL
  stopifnot(is.data.table(x))

  if(x[,class(.SD[[1]])[1]!=class(.SD[[2]])[1],.SDcols=interval_vars]){
    stop("interval_vars must correspond to columns in x of the same class")
  }

  if(x[,!all(sapply(.SD,is.integer)|sapply(.SD,function(x){class(x)%in% c("IDate")})),.SDcols=interval_vars]){
    stop("interval_vars must correspond to columns in x of class integer or IDate")
  }

  if(x[,any(sapply(.SD,function(x){any(is.na(x))})),.SDcols=interval_vars]){
    stop("values in columns specified by interval_vars must be non-missing")
  }


  #stop if interval starts are before interval ends
  if(x[, sum(.SD[[2]]-.SD[[1]] <0)!=0,.SDcols=interval_vars]){
    stop("there exist values in x[[interval_vars[1] ]] that are
         less than corresponding values in  x[[interval_vars[2] ]].
         interval_vars must specify columns corresponding to increasing intervals")
  }


  is_not_preferred_keyx <- !identical(key(x), c(group_vars,interval_vars))
  if(is_not_preferred_keyx){
    statex <- savestate(x)
    if(verbose){message("setkeyv(x,c(group_vars,interval_vars)) prior to calling is.overlap is recommended to save unnecessary row reordering")}
    on.exit(setstate(x,statex))
  }

  data.table::setkeyv(x,c(group_vars,interval_vars))

  any(x[, list(Cisoverlapping(.SD[[1]],.SD[[2]])),by=group_vars,.SDcols=interval_vars][, V1])
}

