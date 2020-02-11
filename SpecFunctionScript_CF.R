##Script to analyze spectrophotometer files

#Gather all files
myfiles <- list.files("./Data", pattern="20070725") #obtain files in Data folder with the pattern "20070725"
class(myfiles) #check the class of the files
myfiles[1] #you can now index any file from the list
myfiles <- paste("Data/", myfiles, sep="") #add Data/ to the beginning of each file name to pull the file out of the Data folder in this directory

##Make a function that reads in the file, names the columns "Lambda" and "Intensity", then only includes values for Lambda that are between 300 and 700
read.spec <- function ( y )  {  
	dat <- read.table(file=y, skip=17, comment.char=">")
	names(dat) <- c("Lambda", "Intensity")
	
	dat <- dat[ dat$Lambda >= 300 & dat$Lambda <= 700,   ]
	return(dat)
}

dat <- read.spec( myfiles[1] )
	
plot.spec <- function( X ) {
	plot(X, type="l")
	maximum <- max(X[2])# find the max intensity, make sure column1 is lambda and column2 is intensity
	maxi <- X[X[2] == maximum]# find the lambda @max intensity
	points(maxi[1], maxi[2], cex=2, col="red")# add that point to the plot
	print(maxi) #print lambda at max intensity for each file
	
	}
	
##Plot each file using the above function and save them all to a PDF
pdf(file="spec_plots_CF.pdf")
	for (i in 1:length(myfiles))
	{ 
	dat <- read.spec(myfiles[i])
	plot.spec(dat)
	}
	dev.off()

	
dat <- read.spec( myfiles[1] ) 
maximum <- max(dat[2])# find the max intensity, make sure column1 is lambda and column2 is intensity
maxi <- dat[dat[2] == maximum]# find the lambda @max intensity
spec_data_CF <- data.frame(myfiles[1], maxi[1], maxi[2]) #create data frame with File name, Lambda, and Intensity
names(spec_data_CF) <- c("File name", "Lambda", "Intensity") #Name each column
	
##Make a for loop that repeats the chunk above for the rest of the files, starting from the second file
for (i in 2:length(myfiles))
	{ 
	dat <- read.spec( myfiles[i] )
	maximum <- max(dat[2])
	maxi <- dat[dat[2] == maximum]
	spec_dataa <- data.frame(myfiles[i], maxi[1], maxi[2]) #make new file name and values into a data frame
	names(spec_dataa) <- c("File name", "Lambda", "Intensity") #Give that data frame the same column headers
	spec_data_CF <- rbind(spec_data_CF, spec_dataa) #bind this data frame to the existing data frame
	}
write.csv(spec_data_CF, file="spec_data_CF.csv") #write a csv with all the File names, Lamda, and Intensity values
	