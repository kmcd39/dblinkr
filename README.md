# dblinkr
Convenience functions for connecting to postgis/postgres database and doing spatial queries. Includes tutorial vignette on working with dbs from R.

# Installation #

To install the package from Github:

```r
# install.packages("devtools")
devtools::install_github("https://github.com/kmcd39/dblinkr.git")
```

Make sure you have a VPN open as well as an account with Princeton Research Computing.

# Usage #

Load the package:

```r
library(dblinkr)
```

Create a database connection:

```r
con <- princeton.db.connect(usr, pw)
```

Take a look at the schemas and tables present:

```r
DBI::dbListObjects(con)
dblinkr::tbls.in.schema(con, "divs")
```

# Longer Tutorial #
A longer tutorial on using dblinkr and working with databases from R generally is here:
https://rpubs.com/kmc39/672009
