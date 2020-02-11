### Script to analyze spectrophotometer files
### Written by ... 

## Function read.spec reads in data, 
## removes header and footer, 
## subsets to wavelength range 300 - 750 

## If you need to setwd() make sure you comment it out before you submit it.

read.spec <- function ( myfile )  {  
	dat <- read.table(file=myfile, skip=17, comment.char=">")
	names(dat) <- c("lambda", "intensity")
	
	dat <- dat[ dat$lambda >= 300 & dat$lambda <= 700,   ]
	return (dat)
}

dat <- read.spec( "Data/20070725_01forirr.txt"  )

plot.spec <- function( X ) {

    plot(X, type="l")
	# find the max intensity
	# find the lambda @max intensity, may want to use indexing
	# add that point to the plot
	# return (lamda, intensity) @max intensity
} 

	## What should we return for our final dataframe of max intensity, lambda?

plot(dat, type="l")   ## ... 




  