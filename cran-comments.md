# v 0.0.0.9003

## Test environments
* local Debian 9 R 3.6.2
* Ubuntu 16.04.6 (on travis-ci) R 4.0.0

### R CMD check results
There were no ERRORs, WARNINGS, or NOTEs

## Test environments:
* win-builder (devel and release)

### R CMD check results
There were ERRORs due to failed checks, traced to an issue with `data.table::foverlaps` reported here: https://github.com/Rdatatable/data.table/issues/4580
