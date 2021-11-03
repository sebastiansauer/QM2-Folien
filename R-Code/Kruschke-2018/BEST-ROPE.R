# File name: BEST-ROPE.R
# Online archive: https://osf.io/jwd3t/
# Author: John K. Kruschke  
# Date: February 25, 2018 
# Contact: johnkruschke@gmail.com  http://www.indiana.edu/~kruschke/
#
### ***************************************************************
### ******* SEE FILE BEST-ROPE-example.R FOR INSTRUCTIONS *********
### ***************************************************************

# Load various essential functions:
source("DBDA2E-utilities.R") 

# Modified plotPost() function from utilities of DBDA2E,
# https://sites.google.com/site/doingbayesiandataanalysis/ 
if( exists("plotPost") ) { rm("plotPost") }
plotPost = function( paramSampleVec , cenTend=c("mode","median","mean")[1] , 
                     compVal=NULL, ROPE=NULL, credMass=0.95, HDItextPlace=0.7, 
                     xlab=NULL , xlim=NULL , yaxt=NULL , ylab=NULL , 
                     main=NULL , cex=NULL , cex.lab=NULL ,
                     col=NULL , border=NULL , showCurve=TRUE , breaks=NULL , 
                     densityBW="nrd0" , ... ) {
  # Override defaults of hist function, if not specified by user:
  # (additional arguments "..." are passed to the hist function)
  if ( is.null(xlab) ) xlab="Param. Val."
  if ( is.null(cex.lab) ) cex.lab=1.5
  if ( is.null(cex) ) cex=1.4
  if ( is.null(xlim) ) xlim=range( c( compVal , ROPE , paramSampleVec ) )
  if ( is.null(main) ) main=""
  if ( is.null(yaxt) ) yaxt="n"
  if ( is.null(ylab) ) ylab=""
  if ( is.null(col) ) col="skyblue"
  if ( is.null(border) ) border="white"
  
  # convert coda object to matrix:
  if ( class(paramSampleVec) == "mcmc.list" ) {
    paramSampleVec = as.matrix(paramSampleVec)
  }
  
  summaryColNames = c("ESS","mean","median","mode",
                      "hdiMass","hdiLow","hdiHigh",
                      "compVal","pGtCompVal",
                      "ROPElow","ROPEhigh","pLtROPE","pInROPE","pGtROPE")
  postSummary = matrix( NA , nrow=1 , ncol=length(summaryColNames) , 
                        dimnames=list( c( xlab ) , summaryColNames ) )
  
  # require(coda) # for effectiveSize function
  postSummary[,"ESS"] = effectiveSize(paramSampleVec)
  
  postSummary[,"mean"] = mean(paramSampleVec)
  postSummary[,"median"] = median(paramSampleVec)
  
  mcmcDensity = density( paramSampleVec, adjust=1.0 ,
                         kernel=c("gaussian","rectangular","triangular")[1] )
  postSummary[,"mode"] = mcmcDensity$x[which.max(mcmcDensity$y)]
  HDIgrid = HDIofGrid( mcmcDensity$y/sum(mcmcDensity$y) , credMass=credMass )
  
  HDI = HDIofMCMC( paramSampleVec , credMass )
  postSummary[,"hdiMass"]=credMass
  postSummary[,"hdiLow"]=HDI[1]
  postSummary[,"hdiHigh"]=HDI[2]
  
  # Plot histogram.
  cvCol = "darkgreen"
  ropeCol = "darkred"
  if ( is.null(breaks) ) {
    if ( max(paramSampleVec) > min(paramSampleVec) ) {
      breaks = c( seq( from=min(paramSampleVec) , to=max(paramSampleVec) ,
                       by=(HDI[2]-HDI[1])/18 ) , max(paramSampleVec) )
    } else {
      breaks=c(min(paramSampleVec)-1.0E-6,max(paramSampleVec)+1.0E-6)
      border="skyblue"
    }
  }
  if ( !showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , xlab=xlab , yaxt=yaxt , ylab=ylab ,
                     freq=F , border=border , col=col ,
                     xlim=xlim , main=main , cex=cex , cex.lab=cex.lab ,
                     breaks=breaks , ... )
  }
  if ( showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , plot=FALSE )
    plot( mcmcDensity$x , mcmcDensity$y , type="l" , lwd=1 , col=col , bty="n" ,
          xlim=xlim , xlab=xlab , yaxt=yaxt , ylab=ylab ,
          main=main , cex=cex , cex.lab=cex.lab , ... )
    polygon( mcmcDensity$x , mcmcDensity$y , col=col , border=col )
    # plot HDI grid points:
    points( mcmcDensity$x[HDIgrid$indices] , 
            rep(0,length(mcmcDensity$x[HDIgrid$indices])) , 
            pch="|" , cex=0.75 )
    text( mean(mcmcDensity$x[HDIgrid$indices]) , 0 , 
          # bquote(.(100*round(HDIgrid$mass,3)) * "% HDI" ) , # actual mass
          bquote(.(100*round(credMass,3)) * "% HDI" ) , # desired mass
          adj=c(.5,-1.2) , cex=cex )
  }
  cenTendHt = 0.9*max(mcmcDensity$y)
  cvHt = 0.7*max(mcmcDensity$y)
  ROPEtextHt = 0.45*max(mcmcDensity$y) # was 0.55*...
  # Display central tendency:
  if ( !is.null(cenTend) ) {
    mn = mean(paramSampleVec)
    med = median(paramSampleVec)
    #mcmcDensity = density(paramSampleVec) # redundant
    mo = mcmcDensity$x[which.max(mcmcDensity$y)]
    if ( cenTend=="mode" ){ 
      text( mo , cenTendHt ,
            bquote(mode==.(signif(mo,3))) , adj=c(.5,0) , cex=cex )
    }
    if ( cenTend=="median" ){ 
      text( med , cenTendHt ,
            bquote(median==.(signif(med,3))) , adj=c(.5,0) , cex=cex , col=cvCol )
    }
    if ( cenTend=="mean" ){ 
      text( mn , cenTendHt ,
            bquote(mean==.(signif(mn,3))) , adj=c(.5,0) , cex=cex )
    }
  }
  # Display the comparison value.
  if ( !is.null( compVal ) ) {
    pGtCompVal = sum( paramSampleVec > compVal ) / length( paramSampleVec ) 
    pLtCompVal = 1 - pGtCompVal
    lines( c(compVal,compVal) , c(0.96*cvHt,0) , 
           lty="dotted" , lwd=2 , col=cvCol )
    text( compVal , cvHt ,
          bquote( .(round(100*pLtCompVal,1)) * "% < " *
                    .(signif(compVal,3)) * " < " * 
                    .(round(100*pGtCompVal,1)) * "%" ) ,
          adj=c(pLtCompVal,0) , cex=0.8*cex , col=cvCol )
    postSummary[,"compVal"] = compVal
    postSummary[,"pGtCompVal"] = pGtCompVal
  }
  # Display the ROPE.
  if ( !is.null( ROPE ) ) {
    pInROPE = ( sum( paramSampleVec > ROPE[1] & paramSampleVec < ROPE[2] )
                / length( paramSampleVec ) )
    pGtROPE = ( sum( paramSampleVec >= ROPE[2] ) / length( paramSampleVec ) )
    pLtROPE = ( sum( paramSampleVec <= ROPE[1] ) / length( paramSampleVec ) )
    lines( c(ROPE[1],ROPE[1]) , c(0.90*ROPEtextHt,0) , lty="solid" , lwd=2 ,
           col=ropeCol )
    lines( c(ROPE[2],ROPE[2]) , c(0.90*ROPEtextHt,0) , lty="solid" , lwd=2 ,
           col=ropeCol)
    # text( mean(ROPE) , ROPEtextHt ,
    #       bquote( .(round(100*pLtROPE,1)) * "% < " * .(ROPE[1]) * " < " * 
    #                 .(round(100*pInROPE,1)) * "% < " * .(ROPE[2]) * " < " * 
    #                 .(round(100*pGtROPE,1)) * "%" ) ,
    #       adj=c( min(max(pLtROPE+pInROPE,.1),.9) ,0) , cex=1.25 , col=ropeCol )
    text( ROPE[1] , ROPEtextHt , bquote(.(round(100*pLtROPE,1))*"%") , 
          adj=c(1.1,0.6) , cex=1.25 , col=ropeCol )  
    text( ROPE[2] , ROPEtextHt , bquote(.(round(100*pGtROPE,1))*"%") , 
          adj=c(-0.1,0.6) , cex=1.25 , col=ropeCol )  
    text( mean(ROPE) , ROPEtextHt , bquote(.(round(100*pInROPE,1))*"%") , 
          adj=c(0.5,-0.5) , cex=1.25 , col=ropeCol )  
    postSummary[,"ROPElow"]=ROPE[1] 
    postSummary[,"ROPEhigh"]=ROPE[2] 
    postSummary[,"pLtROPE"]=pLtROPE
    postSummary[,"pInROPE"]=pInROPE
    postSummary[,"pGtROPE"]=pGtROPE
  }
  # Display the HDI.
  if (!showCurve) {
    lines( HDI , c(0,0) , lwd=7 , lend=1 )
    text( mean(HDI) , 0 , bquote(.(100*credMass) * "% HDI" ) ,
          adj=c(.5,-1.7) , cex=cex )
    text( HDI[1] , 0 , bquote(.(signif(HDI[1],3))) ,
          adj=c(HDItextPlace,-0.5) , cex=cex )
    text( HDI[2] , 0 , bquote(.(signif(HDI[2],3))) ,
          adj=c(1.0-HDItextPlace,-0.5) , cex=cex )
  }
  par(xpd=F)
  #
  return( postSummary )
}

BESTmcmc = function( y1 , y2 , 
                     priorOnly=FALSE , showMCMC=FALSE ,
                     numSavedSteps=20000 , thinSteps=1 ,
                     mu1PriorMean = mean(c(y1,y2)) ,
                     mu1PriorSD = sd(c(y1,y2))*5 ,
                     mu2PriorMean = mean(c(y1,y2)) ,
                     mu2PriorSD = sd(c(y1,y2))*5 ,
                     sigma1PriorMode = sd(c(y1,y2)) ,
                     sigma1PriorSD = sd(c(y1,y2))*5 ,
                     sigma2PriorMode = sd(c(y1,y2)) ,
                     sigma2PriorSD = sd(c(y1,y2))*5 ,
                     nuPriorMean = 30 ,
                     nuPriorSD = 30 ,
                     runjagsMethod=runjagsMethodDefault , 
                     nChains=nChainsDefault ) { 
  # This function generates an MCMC sample from the posterior distribution.
  # Description of arguments:
  # showMCMC is a flag for displaying diagnostic graphs of the chains. If FALSE
  # (the default), no chain graphs are displayed. If TRUE, they are.
  
  #------------------------------------------------------------------------------
  # THE DATA.
  # Load the data:
  y = c( y1 , y2 ) # combine data into one vector
  x = c( rep(1,length(y1)) , rep(2,length(y2)) ) # create group membership code
  Ntotal = length(y)
  # Specify the data and prior constants in a list, for later shipment to JAGS:
  if ( priorOnly ) {
    dataList = list(
      #  y = y ,
      x = x ,
      Ntotal = Ntotal ,
      mu1PriorMean = mu1PriorMean ,
      mu1PriorSD = mu1PriorSD ,
      mu2PriorMean = mu2PriorMean ,
      mu2PriorSD = mu2PriorSD ,
      Sh1 = gammaShRaFromModeSD( mode=sigma1PriorMode , 
                                 sd=sigma1PriorSD )$shape ,
      Ra1 = gammaShRaFromModeSD( mode=sigma1PriorMode , 
                                 sd=sigma1PriorSD )$rate ,
      Sh2 = gammaShRaFromModeSD( mode=sigma2PriorMode , 
                                 sd=sigma2PriorSD )$shape ,
      Ra2 = gammaShRaFromModeSD( mode=sigma2PriorMode , 
                                 sd=sigma2PriorSD )$rate ,
      ShNu = gammaShRaFromMeanSD( mean=nuPriorMean , sd=nuPriorSD )$shape ,
      RaNu = gammaShRaFromMeanSD( mean=nuPriorMean , sd=nuPriorSD )$rate
    )
  } else {
    dataList = list(
      y = y ,
      x = x ,
      Ntotal = Ntotal ,
      mu1PriorMean = mu1PriorMean ,
      mu1PriorSD = mu1PriorSD ,
      mu2PriorMean = mu2PriorMean ,
      mu2PriorSD = mu2PriorSD ,
      Sh1 = gammaShRaFromModeSD( mode=sigma1PriorMode , 
                                 sd=sigma1PriorSD )$shape ,
      Ra1 = gammaShRaFromModeSD( mode=sigma1PriorMode , 
                                 sd=sigma1PriorSD )$rate ,
      Sh2 = gammaShRaFromModeSD( mode=sigma2PriorMode , 
                                 sd=sigma2PriorSD )$shape ,
      Ra2 = gammaShRaFromModeSD( mode=sigma2PriorMode , 
                                 sd=sigma2PriorSD )$rate ,
      ShNu = gammaShRaFromMeanSD( mean=nuPriorMean , sd=nuPriorSD )$shape ,
      RaNu = gammaShRaFromMeanSD( mean=nuPriorMean , sd=nuPriorSD )$rate
    )
  }
  
  #----------------------------------------------------------------------------
  # THE MODEL.
  modelString = "
  model {
    for ( i in 1:Ntotal ) {
      y[i] ~ dt( mu[x[i]] , 1/sigma[x[i]]^2 , nu )
    }
    mu[1] ~ dnorm( mu1PriorMean , 1/mu1PriorSD^2 )  # prior for mu[1]
    sigma[1] ~ dgamma( Sh1 , Ra1 )     # prior for sigma[1]
    mu[2] ~ dnorm( mu2PriorMean , 1/mu2PriorSD^2 )  # prior for mu[2]
    sigma[2] ~ dgamma( Sh2 , Ra2 )     # prior for sigma[2]
    nu ~ dgamma( ShNu , RaNu ) # prior for nu
  }
  " # close quote for modelString
  # Write out modelString to a text file
  writeLines( modelString , con="BESTmodel.txt" )
  
  #------------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Initial values of MCMC chains based on data:
  mu = c( mean(y1) , mean(y2) )
  sigma = c( sd(y1) , sd(y2) )
  # Regarding initial values in next line: (1) sigma will tend to be too big if 
  # the data have outliers, and (2) nu starts at 5 as a moderate value. These
  # initial values keep the burn-in period moderate.
  initsList = list( mu = mu , sigma = sigma , nu = 5 )
  
  #------------------------------------------------------------------------------
  # RUN THE CHAINS
  
  parameters = c( "mu" , "sigma" , "nu" )     # The parameters to be monitored
  adaptSteps = 500               # Number of steps to "tune" the samplers
  burnInSteps = 1000
  
  runJagsOut <- run.jags( method=runjagsMethod ,
                          model="BESTmodel.txt" , 
                          monitor=parameters , 
                          data=dataList ,  
                          inits=initsList , 
                          n.chains=nChains ,
                          adapt=adaptSteps ,
                          burnin=burnInSteps , 
                          sample=ceiling(numSavedSteps/nChains) ,
                          thin=thinSteps ,
                          summarise=FALSE ,
                          plots=FALSE )
  codaSamples = as.mcmc.list( runJagsOut )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  
  ## Original version, using rjags:
  #   nChains = 3 
  #   nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )
  #   # Create, initialize, and adapt the model:
  #   jagsModel = jags.model( "BESTmodel.txt" , data=dataList , inits=initsList , 
  #                           n.chains=nChains , n.adapt=adaptSteps )
  #   # Burn-in:
  #   cat( "Burning in the MCMC chain...\n" )
  #   update( jagsModel , n.iter=burnInSteps )
  #   # The saved MCMC chain:
  #   cat( "Sampling final MCMC chain...\n" )
  #   codaSamples = coda.samples( jagsModel , variable.names=parameters , 
  #                               n.iter=nIter , thin=thinSteps )
  
  #------------------------------------------------------------------------------
  # EXAMINE THE RESULTS
  if ( showMCMC ) {
    for ( paramName in varnames(codaSamples) ) {
      diagMCMC( codaSamples , parName=paramName ,
                saveName=paste0("BEST-ROPE-diag-",paramName) , 
                saveType="png" )
    }
  }
  
  # Convert coda-object codaSamples to matrix object for easier handling.
  # But note that this concatenates the different chains into one long chain.
  # Result is mcmcChain[ stepIdx , paramIdx ]
  mcmcChain = as.matrix( codaSamples )
  return( mcmcChain )
  
} # end function BESTmcmc

#==============================================================================

BESTsummary = function( y1 , y2 , mcmcChain ) {
  #source("HDIofMCMC.R") # in DBDA2E-utilities.R
  mcmcSummary = function( paramSampleVec , credMass=0.95 , compVal=NULL ) {
    meanParam = mean( paramSampleVec )
    medianParam = median( paramSampleVec )
    dres = density( paramSampleVec )
    modeParam = dres$x[which.max(dres$y)]
    hdiLim = HDIofMCMC( paramSampleVec , credMass=credMass )
    if ( !is.null(compVal) ) {
      pcgtCompVal = ( 100 * sum( paramSampleVec > compVal ) 
                      / length( paramSampleVec ) )
    } else {
      pcgtCompVal=NA
    }
    require(coda) 
    ESS = effectiveSize( paramSampleVec )
    return( c( meanParam , medianParam , modeParam , 
               credMass , hdiLim , ESS , pcgtCompVal ) )
  }
  # Define matrix for storing summary info:
  summaryInfo = matrix( 0 , nrow=9 , ncol=8 , dimnames=list(
    PARAMETER=c( "mu1" , "mu2" , "muDiff" , "sigma1" , "sigma2" , "sigmaDiff" ,
                 "nu" , "nuLog10" , "effSz" ),
    SUMMARY.INFO=c( "mean" , "median" , "mode" , 
                    "HDImass" , "HDIlow" , "HDIhigh" , "ESS" , "pcGtZero" ) 
  ) )
  summaryInfo[ "mu1" , ] = mcmcSummary( mcmcChain[,"mu[1]"] )
  summaryInfo[ "mu2" , ] = mcmcSummary( mcmcChain[,"mu[2]"] )
  summaryInfo[ "muDiff" , ] = mcmcSummary( mcmcChain[,"mu[1]"]
                                           - mcmcChain[,"mu[2]"] , 
                                           compVal=0 )
  summaryInfo[ "sigma1" , ] = mcmcSummary( mcmcChain[,"sigma[1]"] )
  summaryInfo[ "sigma2" , ] = mcmcSummary( mcmcChain[,"sigma[2]"] )
  summaryInfo[ "sigmaDiff" , ] = mcmcSummary( mcmcChain[,"sigma[1]"]
                                              - mcmcChain[,"sigma[2]"] , 
                                              compVal=0 )
  summaryInfo[ "nu" , ] = mcmcSummary( mcmcChain[,"nu"] )
  summaryInfo[ "nuLog10" , ] = mcmcSummary( log10(mcmcChain[,"nu"]) )
  
  N1 = length(y1)
  N2 = length(y2)
  effSzChain = ( ( mcmcChain[,"mu[1]"] - mcmcChain[,"mu[2]"] ) 
                 / sqrt( ( mcmcChain[,"sigma[1]"]^2 
                           + mcmcChain[,"sigma[2]"]^2 ) / 2 ) ) 
  summaryInfo[ "effSz" , ] = mcmcSummary( effSzChain , compVal=0 )
  # Or, use sample-size weighted version:
  # effSz = ( mu1 - mu2 ) / sqrt( ( sigma1^2 *(N1-1) + sigma2^2 *(N2-1) ) 
  #                               / (N1+N2-2) )
  # Be sure also to change plot label in BESTplot function, below.
  return( summaryInfo )
}

#==============================================================================

BESTplot = function( y1 , y2 , mcmcChain , ROPEm=NULL , ROPEsd=NULL , 
                     ROPEeff=NULL , showCurve=FALSE , pairsPlot=FALSE ) {
  # This function plots the posterior distribution (and data).
  # Description of arguments:
  # y1 and y2 are the data vectors.
  # mcmcChain is a list of the type returned by function BTT.
  # ROPEm is a two element vector, such as c(-1,1), specifying the limit
  #   of the ROPE on the difference of means.
  # ROPEsd is a two element vector, such as c(-1,1), specifying the limit
  #   of the ROPE on the difference of standard deviations.
  # ROPEeff is a two element vector, such as c(-1,1), specifying the limit
  #   of the ROPE on the effect size.
  # showCurve is TRUE or FALSE and indicates whether the posterior should
  #   be displayed as a histogram (by default) or by an approximate curve.
  # pairsPlot is TRUE or FALSE and indicates whether scatterplots of pairs
  #   of parameters should be displayed.
  mu1 = mcmcChain[,"mu[1]"]
  mu2 = mcmcChain[,"mu[2]"]
  sigma1 = mcmcChain[,"sigma[1]"]
  sigma2 = mcmcChain[,"sigma[2]"]
  nu = mcmcChain[,"nu"]
  if ( pairsPlot ) {
    # Plot the parameters pairwise, to see correlations:
    openGraph(width=7,height=7)
    nPtToPlot = 1000
    plotIdx = floor(seq(1,length(mu1),by=length(mu1)/nPtToPlot))
    panel.cor = function(x, y, digits=2, prefix="", cex.cor, ...) {
      usr = par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r = (cor(x, y))
      txt = format(c(r, 0.123456789), digits=digits)[1]
      txt = paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex=1.25 ) # was cex=cex.cor*r
    }
    pairs( cbind( mu1 , mu2 , sigma1 , sigma2 , log10(nu) )[plotIdx,] ,
           labels=c( expression(mu[1]) , expression(mu[2]) , 
                     expression(sigma[1]) , expression(sigma[2]) , 
                     expression(log10(nu)) ) , 
           lower.panel=panel.cor , col="skyblue" )
  }
  # source("plotPost.R") # in DBDA2E-utilities.R
  # Set up window and layout:
  openGraph(width=6.0,height=9.0)
  layout( matrix( c(4,5,7,8,3,1,2,6,9,10) , nrow=5, byrow=FALSE ) )
  par( mar=c(3.5,3.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  
  # Select thinned steps in chain for plotting of posterior predictive curves:
  chainLength = NROW( mcmcChain )
  nCurvesToPlot = 30
  stepIdxVec = seq( 1 , chainLength , floor(chainLength/nCurvesToPlot) )
  xRange = range( c(y1,y2) )
  if ( isTRUE( all.equal( xRange[2] , xRange[1] ) ) ) {
    meanSigma = mean( c(sigma1,sigma2) )
    xRange = xRange + c( -meanSigma , meanSigma )
  }
  xLim = c( xRange[1]-0.1*(xRange[2]-xRange[1]) , 
            xRange[2]+0.1*(xRange[2]-xRange[1]) )
  xVec = seq( xLim[1] , xLim[2] , length=200 )
  maxY = max( dt( 0 , df=max(nu[stepIdxVec]) ) /
                min(c(sigma1[stepIdxVec],sigma2[stepIdxVec])) )
  # Plot data y1 and smattering of posterior predictive curves:
  stepIdx = 1
  plot( xVec , dt( (xVec-mu1[stepIdxVec[stepIdx]])/sigma1[stepIdxVec[stepIdx]] , 
                   df=nu[stepIdxVec[stepIdx]] )/sigma1[stepIdxVec[stepIdx]] , 
        ylim=c(0,maxY) , cex.lab=1.75 ,
        type="l" , col="skyblue" , lwd=1 , xlab="y" , ylab="p(y)" , 
        main="Data Group 1 w. Post. Pred." )
  for ( stepIdx in 2:length(stepIdxVec) ) {
    lines(xVec, dt( (xVec-mu1[stepIdxVec[stepIdx]])/sigma1[stepIdxVec[stepIdx]] , 
                    df=nu[stepIdxVec[stepIdx]] )/sigma1[stepIdxVec[stepIdx]] , 
          type="l" , col="skyblue" , lwd=1 )
  }
  histBinWd = median(sigma1)/2
  histCenter = mean(mu1)
  histBreaks = sort( c( seq( histCenter-histBinWd/2 , min(xVec)-histBinWd/2 ,
                             -histBinWd ),
                        seq( histCenter+histBinWd/2 , max(xVec)+histBinWd/2 ,
                             histBinWd ) , xLim ) )
  histInfo = hist( y1 , plot=FALSE , breaks=histBreaks )
  yPlotVec = histInfo$density 
  yPlotVec[ yPlotVec==0.0 ] = NA
  xPlotVec = histInfo$mids
  xPlotVec[ yPlotVec==0.0 ] = NA
  points( xPlotVec , yPlotVec , type="h" , lwd=11 , col="pink" , lend=1 )
  text( max(xVec) , maxY , bquote(N[1]==.(length(y1))) , adj=c(1.1,1.1) )
  # Plot data y2 and smattering of posterior predictive curves:
  stepIdx = 1
  plot( xVec , dt( (xVec-mu2[stepIdxVec[stepIdx]])/sigma2[stepIdxVec[stepIdx]] , 
                   df=nu[stepIdxVec[stepIdx]] )/sigma2[stepIdxVec[stepIdx]] , 
        ylim=c(0,maxY) , cex.lab=1.75 , 
        type="l" , col="skyblue" , lwd=1 , xlab="y" , ylab="p(y)" , 
        main="Data Group 2 w. Post. Pred." )
  for ( stepIdx in 2:length(stepIdxVec) ) {
    lines(xVec, dt( (xVec-mu2[stepIdxVec[stepIdx]])/sigma2[stepIdxVec[stepIdx]] , 
                    df=nu[stepIdxVec[stepIdx]] )/sigma2[stepIdxVec[stepIdx]] , 
          type="l" , col="skyblue" , lwd=1 )
  }
  histBinWd = median(sigma2)/2
  histCenter = mean(mu2)
  histBreaks = sort( c( seq( histCenter-histBinWd/2 , min(xVec)-histBinWd/2 ,
                             -histBinWd ),
                        seq( histCenter+histBinWd/2 , max(xVec)+histBinWd/2 ,
                             histBinWd ) , xLim ) )
  histInfo = hist( y2 , plot=FALSE , breaks=histBreaks )
  yPlotVec = histInfo$density 
  yPlotVec[ yPlotVec==0.0 ] = NA
  xPlotVec = histInfo$mids
  xPlotVec[ yPlotVec==0.0 ] = NA
  points( xPlotVec , yPlotVec , type="h" , lwd=11 , col="pink" , lend=1 )
  text( max(xVec) , maxY , bquote(N[2]==.(length(y2))) , adj=c(1.1,1.1) )
  
  # Plot posterior distribution of parameter nu:
  histInfo = plotPost( log10(nu) , col="skyblue" , # breaks=30 ,
                       showCurve=showCurve ,
                       xlab=bquote("log10("*nu*")") , cex.lab = 1.75 , 
                       cenTend=c("mode","median","mean")[1] ,
                       main="Normality" ) #  (<0.7 suggests kurtosis)
  
  # Plot posterior distribution of parameters mu1, mu2, and their difference:
  xlim = range( c( mu1 , mu2 ) )
  histInfo = plotPost( mu1 ,  xlim=xlim , cex.lab = 1.75 ,
                       showCurve=showCurve ,
                       xlab=bquote(mu[1]) , main=paste("Group",1,"Mean") , 
                       col="skyblue" )
  histInfo = plotPost( mu2 ,  xlim=xlim , cex.lab = 1.75 ,
                       showCurve=showCurve ,
                       xlab=bquote(mu[2]) , main=paste("Group",2,"Mean") , 
                       col="skyblue" )
  histInfo = plotPost( mu1-mu2 , 
                       #compVal=0 ,  
                       showCurve=showCurve ,
                       xlab=bquote(mu[1] - mu[2]) , cex.lab = 1.75 , ROPE=ROPEm ,
                       main="Difference of Means" , col="skyblue" )
  
  # Plot posterior distribution of param's sigma1, sigma2, and their difference:
  xlim=range( c( sigma1 , sigma2 ) )
  histInfo = plotPost( sigma1 ,  xlim=xlim , cex.lab = 1.75 ,
                       showCurve=showCurve ,
                       xlab=bquote(sigma[1]) , main=paste("Group",1,"Scale") , 
                       col="skyblue" , cenTend=c("mode","median","mean")[1] )
  histInfo = plotPost( sigma2 ,  xlim=xlim , cex.lab = 1.75 ,
                       showCurve=showCurve ,
                       xlab=bquote(sigma[2]) , main=paste("Group",2,"Scale") , 
                       col="skyblue" , cenTend=c("mode","median","mean")[1] )
  histInfo = plotPost( sigma1-sigma2 , 
                       #compVal=0 ,  
                       showCurve=showCurve ,
                       xlab=bquote(sigma[1] - sigma[2]) , cex.lab = 1.75 , 
                       ROPE=ROPEsd ,
                       main="Difference of Scales" , col="skyblue" , 
                       cenTend=c("mode","median","mean")[1] )
  
  # Plot of estimated effect size. Effect size is d-sub-a from 
  # Macmillan & Creelman, 1991; Simpson & Fitter, 1973; Swets, 1986a, 1986b.
  effectSize = ( mu1 - mu2 ) / sqrt( ( sigma1^2 + sigma2^2 ) / 2 )
  histInfo = plotPost( effectSize , 
                       #compVal=0 ,  
                       ROPE=ROPEeff ,
                       showCurve=showCurve ,
                       xlab=bquote( delta *" = "* (mu[1]-mu[2])
                                    /sqrt((sigma[1]^2 +sigma[2]^2 )/2 ) ),
                       cenTend=c("mode","median","mean")[1] , cex.lab=1.0 , main="Effect Size" , 
                       col="skyblue" )
  # Or use sample-size weighted version:
  # Hedges 1981; Wetzels, Raaijmakers, Jakab & Wagenmakers 2009.
  # N1 = length(y1)
  # N2 = length(y2)
  # effectSize = ( mu1 - mu2 ) / sqrt( ( sigma1^2 *(N1-1) + sigma2^2 *(N2-1) )
  #                                    / (N1+N2-2) )
  # Be sure also to change BESTsummary function, above.
  # histInfo = plotPost( effectSize , compVal=0 ,  ROPE=ROPEeff ,
  #          showCurve=showCurve ,
  #          xlab=bquote( (mu[1]-mu[2])
  #          /sqrt((sigma[1]^2 *(N[1]-1)+sigma[2]^2 *(N[2]-1))/(N[1]+N[2]-2)) ),
  #          cenTend=c("mode","median","mean")[1] , cex.lab=1.0 , main="Effect Size" , col="skyblue" )
  return( BESTsummary( y1 , y2 , mcmcChain ) )
} # end of function BESTplot

#==============================================================================
