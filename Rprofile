

# Some useful aliases
cd <- setwd
pwd <- getwd
lss <- dir

r <- getOption( "repos" ) 
r["CRAN"] <- "http://cran.us.r-project.org"
options( repos=r )
rm(r)



# Override q() to not save by default.
# Same as saying q("no")
q <- function (save="no", ...) {
  quit(save=save, ...)
}

# Andrew Gelman's plot seasoning:
#setHook( 'plot.new', function(..) par (mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.02) )
