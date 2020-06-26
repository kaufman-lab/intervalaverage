# intervalaverage 0.0.0.9001

## POTENTIALLY BREAKING CHANGES

1. Interval columns used by `intervalaverage`, `intervalintersect`,
and `isolateoverlaps` functions (i.e. those columns of `x` and `y`
specified by the `interval_vars` argument in all of those functions)
must now be of either class `integer`
or `IDate`. This is a change from previous where columns of class `Date` were allowed. 
`Date` objects could have hidden decimals which might cause problems in this function
which is written to deal with only discrete time.

## NEW FEATURES

## BUG FIXES