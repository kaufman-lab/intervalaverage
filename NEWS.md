# intervalaverage 0.1 (Release)

## BUG FIXES
1. More minor improvements in documentation (edits to intro vignette)
2. Improvements in code to avoid side-effects (ie changes in row order) that were originally implemented in 0.0.0.9001, based on suggestions from https://github.com/Rdatatable/data.table/issues/4575#issuecomment-650656185
3. intervalintersect now returns columns in a more sensible order.

# intervalaverage 0.0.0.9002 (Development)

## BUG FIXES
1 . Minor improvements in documentation (readme,intro vignette)

# intervalaverage 0.0.0.9001 (Development)

## POTENTIALLY BREAKING CHANGES

1. Interval columns used by `intervalaverage`, `intervalintersect`,
and `isolateoverlaps` functions (i.e. those columns of `x` and `y`
specified by the `interval_vars` argument in all of those functions)
must now be of either class `integer`
or `IDate`. This is a change from previous where columns of class `Date` were allowed. 
`Date` objects could have hidden decimals which might cause problems in this function
which is written to deal with only discrete time. Changing Dates to IDates should fix existing code.


2. `intervalaverage` now restores the keys of x and y to their state prior to the function call if x and y are not already keyed on c(group_vars,interval_vars) and prints a message recommending that the keys be set first (to avoid unsetting the key at the end of the function if you don't care about the order). This is a breaking change only if code relied on `intervalaverage` changing the key.




## NEW FEATURES

## BUG FIXES

1. `isolateoverlaps` no longer sets the key of x internal to the function as this was never necessary for the foverlaps call to work. This should reduce the possiblity of side-effects for this function even though x is still passed by reference. This fixes the problem that the key of x might changed if the function returns an error (the function was already written to restore the key of x if it successfully completed).

2. CJ.dt no longer passes-by-references the data.tables provided as arguments in `...`; While this may add some more memory usage and processing time to copy the table, it really shouldn't matter since if you don't have memory to copy a table you don't have memory to cartesian join it with another table.

3. `is.overlapping` now restores the key and column order of x to its original state even if the function returns an error.
