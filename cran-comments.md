# ExclusionTable 1.0.1

## R CMD check results
ERROR 0 | WARNINGS 0 | NOTES 0

## Downstream dependencies
There are currently no downstream dependencies for this package.

# ExclusionTable 1.0.0

## Resubmission
This is a resubmission. In this version I have:
  - Removed quote from function names in the description file
  - Added "https://" to the beginning of all web links and added trailing slashes.
  - Added a description of the return value of `print.exl_tbl()`.

## R CMD check results
There were no ERRORs, or WARNINGs.

There was one NOTE on the Windows Server 2022, R-devel, 64 bit:

```
* checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException
```

This can likely be ignored as discussed in r-hub issue [503](https://github.com/r-hub/rhub/issues/503).

## Downstream dependencies
There are currently no downstream dependencies for this package.
