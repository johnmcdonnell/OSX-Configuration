## Don't echo anything from init
sink("/dev/null")


# Some useful aliases
cd <- setwd
pwd <- getwd

# Don't spam large amounts of data at me
options(max.print = 200)

# Options pertaining to an interactive session
if (interactive()) {
  library( vimcom )
  library( colorout )
  library( setwidth )
  options(vimcom.verbose = 1, vimcom.allnames=TRUE)
  if(Sys.getenv("VIMRPLUGIN_TMPDIR") != "")
    library(vimcom)
  if(Sys.getenv("TERM") != "linux" && Sys.getenv("TERM") != "")
    setOutputColors256(verbose = FALSE)
  if(nchar(Sys.getenv("DISPLAY")) > 1 && Sys.info()["sysname"] != "Darwin"){
     grDevices::X11.options(width = 4.5, height = 4, ypos = 0,
                            xpos = 1000, pointsize = 10)
     options(editor = 'gvim -f -c "set ft=r"')
     options(pager = "gvim -c 'set ft=rdoc' -")
   } else {
     options(editor = 'vim -c "set ft=r"')
     options(pager = "vim -c 'set ft=rdoc' -")
   }
}

addrepo <- function(...) {
      options(repos=unique(c(getOption("repos"), c(...))))
}

#addrepo("http://watson.nci.nih.gov/cran")
addrepo("http://cran.mirrors.hoobly.com/")
addrepo("http://r-forge.r-project.org")
addrepo("http://cran.us.r-project.org")

# Stop asking me which repo to use every time I try to install
# something. (Simply adding a repo is not enough)
options(repos=Filter(function(x) x != "@CRAN@", getOption("repos")))

## Turn echo back on
sink(NULL)

