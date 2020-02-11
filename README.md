# homework-4

## Function Practice and Automated Data Processing

This is an individual assignment. Do not share your work with others.

Stub in `SpecFunctionScript_stub.R`  
Sample plot in `SpecPlotExample.pdf`  
Sample output file in `SampleOutput.csv`

Due: Monday Feb. 10 by midnight

By: GitHub Classroom submission   
Turn in:
* a script `SpecFunctionScript_myinitials.R`  
* Output plots  
* Output file in `.csv` format

## The problem 

A light spectroradiometer is an instrument that measures light intensity with a probe and records the data as intesity by wavelength. You can use it to take environmental spectral readings that are important both for visual systems as well as photosynthesis. In an actual study, you will end up with many many files, each of which represents one sample, and it would be very helpful to automate data extraction and processing. 

The data files are in the `Data` directory. Before you begin take a look at the input files and think about what you need to do to read in the data. Iʻve helped you out by providing a function in the stub, make sure you understand what itʻs doing.

Your task is to write a script that will:

1. Read in a spectroradiometer file
2. Subset it to a relevant wavelength range. Most animals can only see in the near UV-red (about 300-700nm)
3. Plot intensity as a function of wavelength
4. Find the max intensity and wavelength at which it occurs
5. Automate it by reading in the filenames, and passing the filenames to a function that will automate the data processing.
6. Collect the filename, max intensity, and wavelengths into a dataframe and output to file. 

__Check out the input and output files to better understand the task__

## Some hints

1. The first step is to make sure that the two functions `read.spec()` and `plot.spec()` work as expected. Complete the functions and debug it to get it to work.
2. `plot.spec()`should take in a dataframe for one file, create a lineplot of intensity as a function of wavelength, find the point on the plot where maximum intensity occurs, and plot a red point there. It should also return the coordinates where maximum intensity occurs (intensity and wavelength). 
3. Use the `list.files()` function to get all the files in your `Data` directory and save as object `myfiles`. What is the class of `myfiles`?  Use `grep()` to get all the files that are data files only and no other files (you should have other files in your `Data` directory. How many data files are there? This also is a way to *error-proof* your code to make sure you have valid input.
4. Because your files are in the `Data` directory, add the path to each element of `myfiles` using the `paste()` function. Donʻt forget to include the `\`. Consult help `?paste` if needed. 
5. Figure out how to use `myfiles` in your script to read in data without typing in filenames!
6. Write a wrapper function that will read in a file, create the plot, and return the max intensity and lambda for that file. It will be a very short function. Execute once for each file, and save the output as a dataframe with one row per file. 
7. Instead of plotting to console, generate pdfs, one for each file. To do this, you can just open the pdf device `pdf()` outside of the call to the wrapper function, and turn off the device afterwards. It will create a pdf with multiple pages, one for each time a plot is created.
8. Write the final dataframe to an output file. It must look like a completed `SampleOutput.csv` with one row per input file. (Note: lambda is the symbol for wavelength.) It might be easier to combine the `myfiles` with the output data (`lambda` and `max_intensity`), but you can use any strategy you wish.
9. Clean up your code and delete any unnecessary code and comments. Organize your script to put all function definitions at the top of the script, with all the code that does the actual work below it. Use comments to label  these sections.  Make sure you have good comments for each major step, and that the code is readable. Test it by sourcing it from the R console 

Just FYI, in case you are wondering, there are two types of readings in this collection of files collecting irradiance and radiance in mutiple directions. Irradiance is a measure of all light hitting the probe in a 180 degree hemisphere. Radiance is is a measure of reflected light, in a narrow angular sample. The filenames contain "up" "left" "right" and "forward" for samples from the probe pointed in different directions relative to a perching location of a damselfly. 

## Congratulations! You wrote a beautiful script!

