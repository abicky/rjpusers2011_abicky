library(RUnit)

getFilePath <- function() {
   filename <- commandArgs()[4]
   if (is.na(filename)) {
       # cf. http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
       frame.files <- lapply(sys.frames(), function(x) x$ofile)
       frame.files <- Filter(Negate(is.null), frame.files)
       filename <- frame.files[[length(frame.files)]]
   } else {
       filename <- sub("^--file=", "", filename)
   }
   filename
}

dir <- dirname(getFilePath())
source(sprintf("%s/utils.R", dir))
result <- runTestFile(sprintf("%s/t/utils.t", dir))
print(result)
