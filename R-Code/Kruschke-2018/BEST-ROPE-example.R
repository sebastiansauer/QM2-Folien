
# File name: BEST-ROPE-example.R
# Online archive: https://osf.io/jwd3t/
# Author: John K. Kruschke  
# Date: February 25, 2018 
# Contact: johnkruschke@gmail.com  http://www.indiana.edu/~kruschke/
#
# This R script was used to create the example depicted in Figure 2 of the
# article: Kruschke, J. K. (2018). Rejecting or accepting parameter values in
# Bayesian estimation. Advances in Methods and Practices in Psychological
# Science. 
#
# This script and its accompanying scripts (explained below) are small
# variations of the R scripts used for two-group comparison that accompany the
# article: Kruschke, J. K. (2013). Bayesian estimation supersedes the t test.
# Journal of Experimental Psychology: General, v.142(2), pp.573-603 (doi:
# 10.1037/a0029146). The original R code for that article is available at
# http://www.indiana.edu/~kruschke/BEST/
#
# This program is believed to be free of errors, but it comes with no guarantee! 
# The user bears all responsibility for interpreting the results. 
#
### To run this program, please do the following:
### 1. Install the general-purpose programming language R from  
###      http://www.r-project.org/
###    Install the version of R appropriate for your computer's operating
###    system (Windows, MacOS, or Linux).   
### 2. Install the Bayesian MCMC sampling program JAGS from
###      http://mcmc-jags.sourceforge.net/
###    Again, intall the version appropriate for your operating system.
### 3. Install the R editor, RStudio, from
###      http://rstudio.org/
###    This editor is not necessary, but highly recommended.
### 4. Make sure that the following programs are all in the same folder 
###    as this file:
###      BEST-ROPE-example.R (this file)
###      BEST-ROPE.R (which defines functions for JAGS, etc.)
###      DBDA2E-utilities.R (which defines functions for graphics, etc.)
### 5. Make sure that R's working directory is the folder in which those 
###    files reside. In RStudio, use menu tabs Tools -> Set Working Directory.
###    If working in R, use menu tabs File -> Change Dir.
### 6. After the above actions are accomplished, this program should
###    run as-is in R. You may "source" it to run the whole thing at once,
###    or, preferably, run lines consecutively from the beginning.
################################################################################

# Optional: Clear R's memory and graphics:
rm(list=ls())  # Careful! This clears all of R's memory!
graphics.off() # This closes all of R's graphics windows.

#-----------------------------------------------------------------------------
# Required: Get the functions and packages loaded into R's working memory:
source("BEST-ROPE.R") # N.B.: This in turn sources DBDA2E-utilities.R

#-----------------------------------------------------------------------------
# Specify data as vectors. Replace with your own data as desired. 

# Example of explicit vectors (not used here):
# y1 = c(101,100,102,104,102,97,105,105,98,101,100,123,105,103,100,95,102,106,
#        109,102,82,102,100,102,102,101,102,102,103,103,97,97,103,101,97,104,
#        96,103,124,101,101,100,101,101,104,100,101)
# y2 = c(99,101,100,101,102,100,97,101,104,101,102,102,100,105,88,101,100,
#        104,100,100,100,101,102,103,97,101,101,100,101,99,101,100,100,
#        101,100,99,101,100,102,99,100,99)

# Generate random data vectors y1 and y2 for example in article:
sdN = function(x){sqrt(var(x)*(length(x)-1)/length(x))}
standardize = function(x){ (x-mean(x))/sdN(x) }
graphics.off()
Nin = 468 
mu1 = 104
mu2 = 100
sd1 = 15
sd2 = 15
set.seed(47405)
y1 = rnorm( Nin+30 ) ; y1 = standardize(y1)*sd1+mu1
y2 = rnorm( Nin ) ; y2 = standardize(y2)*sd2+mu2
# outliers:
pOut = 0.0
y1out = runif( floor((Nin+30)*pOut) ) 
y1 = c( y1 , standardize(y1out)*sd1*2 + mu1 )
y1 = pmin( pmax( 0 , y1 ) , 200 )
y2out = runif( floor((Nin)*pOut) ) 
y2 = c( y2 , standardize(y2out)*sd2*2 + mu2 )
y2 = pmin( pmax( 0 , y2 ) , 200 )
# The above lines of code merely generate data vectors y1 and y2, used below.

#----------------------------------------------------------------------------
# Run the Bayesian analysis using the default broad prior:
postMcmcChain = BESTmcmc( y1 , y2 , 
                          priorOnly=FALSE ,  # show the posterior, not prior
                          numSavedSteps=21000 , # total number of MCMC steps
                          thinSteps=10 ,   # thinning (optional)
                          showMCMC=TRUE ) # show MCMC diagnostics
postInfo = BESTplot( y1 , y2 , postMcmcChain , 
                     showCurve=TRUE , # plot smooth distrib not histogram
                     ROPEm=c(-1.5,1.5) ,  # specify ROPE on diff of mu's
                     ROPEsd=c(-1.5,1.5) , # specify ROPE on diff of sigma's
                     ROPEeff=c(-0.1,0.1) , # specify ROPE on effect size
                     pairsPlot=FALSE ) # do not show pairwise plot of parameters
# You can save the plot using the pull-down menu in the R graphics window,
# or by using the following:
saveGraph( file="BEST-ROPE-example-Post" , type="pdf" )
# Show detailed summary info on console, output from BESTplot, above:
show( postInfo ) 
# Save numerical info about posterior:
write.csv( postInfo , file="BEST-ROPE-example-PostInfo.csv" )
## Save the data and results for future use, e.g. for power computation:
save( y1, y2, postMcmcChain, postInfo, file="BEST-ROPE-example-MCMC.Rdata" )
## To re-load the saved data and MCMC chain, type: 
# load( "BEST-ROPE-example-MCMC.Rdata" ) 

#-------------------------------------------------------------------------------
