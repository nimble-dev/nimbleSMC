##  Contains code to run a Liu and West filter.  The filter has a build
##  function (buildLiuWestFilter) and a step function (LWstepNS). Also contains 
##  a function for caluclating useful quantities. The doPars() function
##  is specialized to different variables in the mv objects, and allows
##  these variables to be set / retrieved within the mv objects without
##  explicitly calling the variable name.
##  
##
##  Ideas for implementing:
##  option to skip auxiliary step in filter (Ionides '11), which will also allow for threshold resampling

LWStepVirtual <- nimbleFunctionVirtual(
  run = function(m = integer()) {
    returnType(double())
  }
)

LWSetMeanVirtual <- nimbleFunctionVirtual(
  run = function() {
    returnType()
  }
)

# Has a return_mean method which returns the mean of a normally distributed nimble node.
paramMean <- nimbleFunction(
    name = 'paramMean',
  contains = LWSetMeanVirtual,
  setup = function(model, node){
    ## check that node has a normal distribution!
  },
    run = function() {
        model[[node]] <<- model$getParam(node, 'mean')
    }
)

LWSetParVirtual <- nimbleFunctionVirtual(
  methods = list(
    scalarSet = function(scalars = double(1), mvWset = integer(), m = integer(), ids = integer(1)){},
    vectorSet = function(vectors = double(2), mvWset = integer(), m = integer(), ids = integer(1)){},
    scalarGet = function(mvWset = integer(), m = integer()){returnType(double(1))},
    vectorGet = function(mvWset = integer(), m = integer(), length = integer()){returnType(double(2))}
  )
)

doPars <- nimbleFunction(
    name = 'doPars',
  contains = LWSetParVirtual,
  setup = function(parName, mvWSamples, mvEWSamples) {
  },
  methods = list(
    scalarSet = function(scalars = double(1), mvWset = integer(), m = integer(), ids = integer(1)){
      if(mvWset == 1){
        for(i in 1:m){
          mvWSamples[parName, i][1] <<- scalars[ids[i]]
        }
      }
      else{
        for(i in 1:m){
          mvEWSamples[parName, i][1] <<- scalars[ids[i]]
        }
      }
    },
    vectorSet = function(vectors = double(2), mvWset = integer(), m = integer(), ids = integer(1)){
      if(mvWset == 1){
        for(i in 1:m){
          mvWSamples[parName, i] <<- vectors[,ids[i]]
        }
      }
      else{
        for(i in 1:m){
          mvEWSamples[parName, i] <<- vectors[,ids[i]]
        }
      }
    },
    scalarGet = function(mvWset = integer(), m = integer()){
        ##declare(vecOut, double(1,m))
        vecOut <- numeric(m, init=FALSE)
        if(mvWset == 1){
            for(i in 1:m){
          vecOut[i] <- mvWSamples[parName, i][1]
        }
      }
      else{
        for(i in 1:m){
          vecOut[i] <- mvEWSamples[parName, i][1]
        }
      }
      returnType(double(1))
      return(vecOut)
    },
    vectorGet = function(mvWset = integer(), m = integer(), length = integer()){
        ##declare(matOut, double(2,c(length,m)))
        matOut <- matrix(nrow = length, ncol = m, init=FALSE)
        if(mvWset == 1){
        for(i in 1:m){
          matOut[,i] <- mvWSamples[parName, i]
        }
      }
      else{
        for(i in 1:m){
          matOut[,i] <- mvEWSamples[parName, i]
        }
      }
      returnType(double(2))
      return(matOut)
    } 
  ))


LWStep <- nimbleFunction(
    name = 'LWStep',
  contains = LWStepVirtual,
  setup = function(model, mvWSamples, mvEWSamples, nodes, paramVarDims, iNode, paramNodes, paramVars, names, saveAll, d, silent = FALSE) {
    notFirst <- iNode != 1
    prevNode <- nodes[if(notFirst) iNode-1 else iNode]

    modelSteps <- particleFilter_splitModelSteps(model, nodes, iNode, notFirst)
    prevDeterm <- modelSteps$prevDeterm
    calc_thisNode_self <- modelSteps$calc_thisNode_self
    calc_thisNode_deps <- modelSteps$calc_thisNode_deps
    
    thisNode <- nodes[iNode]
    parDeterm <- model$getDependencies(paramNodes, determOnly=TRUE)
    ## Remove any element of parDeterm that is not needed by prevDeterm, calc_thisNode_self or calc_thisNode_deps
    nodes_that_matter <- c(prevDeterm, calc_thisNode_self, calc_thisNode_deps)
    if(length(parDeterm) > 0) {
        thisDeterm_is_intermediate <- logical(length(parDeterm))
        for(i in seq_along(parDeterm)) {
            theseDeps <- model$getDependencies(parDeterm[i])
            thisDeterm_is_intermediate[i] <- !(parDeterm[i] %in% nodes_that_matter) & any(theseDeps %in% nodes_that_matter)
        }
        parDeterm <- parDeterm[thisDeterm_is_intermediate]
    }
    parAndPrevDeterm <- c(parDeterm, prevDeterm)
    parAndPrevDeterm <- model$expandNodeNames(parAndPrevDeterm, sort = TRUE) ## Ensure sorting, because a node in parDeterm that is also in prevDeterm will have been removed from parDeterm, but that is where it potentially comes first.
    

    isLast <- (iNode == length(nodes))
            
    t <- iNode  # current time point
    # Get names of xs node for current and previous time point (used in copy)
    if(saveAll == 1){
      prevXName <- prevNode    
      thisXName <- thisNode
      currInd <- t
      prevInd <- t-1
    }
    else{
      prevXName <- names    
      thisXName <- names
      currInd <- 1
      prevInd <- 1 
    }
    numParams <- sum(paramVarDims)
    numParamVars <- length(paramVars)
    paramInds <- c(0,cumsum(paramVarDims)) 
    parCalc <- LWparFunc(d, numParams, prevInd) # d is discount factor, default to 0.99  
    singleParam <- (numParams == 1)
    
    ##  varSize keeps track of the size of each parameter we are estimating
    varSize <- rep(0, length(paramInds)-1)
    doVarList <- nimbleFunctionList(LWSetParVirtual)
    for(i in 1:numParamVars){
      doVarList[[i]] <- doPars(paramVars[i], mvWSamples, mvEWSamples)
      varSize[i] <- paramInds[i+1]-paramInds[i]
    }
    
    setMeanList <- nimbleFunctionList(LWSetMeanVirtual)
    allLatentNodes <- model$expandNodeNames(thisNode)
    numLatentNodes <- length(allLatentNodes)
    for(i in 1:numLatentNodes){
      setMeanList[[i]] <- paramMean(model, allLatentNodes[i])
    }

    if(singleParam)
      varSize = c(varSize, 0)  # ensure that varSize is treated as a vector even with only one parameter
  },
  run = function(m = integer()) {
    returnType(double())
    logAuxWts <- numeric(m, init=FALSE)
    ll <- numeric(m, init=FALSE)
    logWts <- numeric(m, init=FALSE)
    ids <- integer(m, 0)
    logPreWts <- numeric(m, init=FALSE)
    tmpPars <- matrix(nrow = numParams, ncol = m, init=FALSE)
    if(notFirst){
       for(j in 1:numParamVars){
         if(varSize[j] == 1){
           tmpPars[paramInds[j+1], ] <- doVarList[[j]]$scalarGet(1,m)
         }
         else{
           tmpPars[(paramInds[j]+1):paramInds[j+1], ] <- doVarList[[j]]$vectorGet(1, m, varSize[j])
         }
       }
       for(i in 1:m){
         logWts[i] <- mvWSamples['wts',i][prevInd]
       }
       meanVec <- parCalc$shrinkMean(m, logWts, tmpPars )  # shrink parameter particles towards mean  
       for(i in 1:m) {
         copy(mvWSamples, model, prevXName, prevNode, row=i)
         values(model, paramNodes) <<- meanVec[,i]
         calculate(model, parAndPrevDeterm) 
         for(j in 1:numLatentNodes){
           setMeanList[[j]]$run()
         }
         calculate(model, calc_thisNode_self)
         logAuxWts[i] <- calculate(model, calc_thisNode_deps)
         if(is.nan(logAuxWts[i])) logAuxWts[i] <- -Inf #check for ok param values
         logPreWts[i] <- logAuxWts[i] + logWts[i] #first resample weights
       }
       maxWt <- max(logPreWts)
       wts <- exp(logPreWts - maxWt)  ## compilation failure if temp variable not created
       rankSample(wts, m, ids, silent)
       # Reassign weights and pars so that cov matrix is calculated correctly
       for(i in 1:m){
         copy(mvWSamples, mvEWSamples, nodes = prevXName, nodesTo = prevXName, row = ids[i], rowTo = i)
         logWts[i] <- mvWSamples['wts',ids[i]][prevInd] 
       }
       for(j in 1:numParamVars){
         if(varSize[j] == 1){
           tmpPars[paramInds[j+1], ] <- doVarList[[j]]$scalarGet(1,m)
         }
         else{
           tmpPars[(paramInds[j]+1):paramInds[j+1], ] <- doVarList[[j]]$vectorGet(1, m, varSize[j])
         }
       }
       parChol <- parCalc$cholesVar(m,logWts,tmpPars) #  calculate MC cov matrix
    }
    for(i in 1:m) {
      if(notFirst) {  
        if(singleParam == 1){
          tmpPars[1,i] <- rnorm(1, meanVec[1,ids[i]],  parChol[1,1])
          values(model, paramNodes) <<- tmpPars[,i]
        } else{
          tmpPars[,i] <- rmnorm_chol(1, meanVec[,ids[i]],  parChol, prec_param=0) 
          values(model, paramNodes) <<- tmpPars[,i]
        }
        copy(mvWSamples, model, nodes = prevXName, nodesTo = prevNode, row = ids[i]) 
      } else{
        simulate(model, paramNodes)
        tmpPars[,i] <- values(model,paramNodes)
      }
      calculate(model, parAndPrevDeterm) 
      simulate(model, calc_thisNode_self)
      copy(model, mvWSamples, nodes = thisNode, nodesTo = thisXName, rowTo = i)
      ll[i]  <- calculate(model, calc_thisNode_deps)
      if(is.nan(ll[i])) ll[i] <- -Inf
      #rescale weights by pre-sampling weight
      if(notFirst)
        logWts[i] <- ll[i] - logAuxWts[ids[i]]
      else logWts[i] <- ll[i]
      mvWSamples['wts',i][currInd] <<- logWts[i] 
      ids[i] <- i
    }
    for(j in 1:numParamVars){
      if(varSize[j] == 1){
        doVarList[[j]]$scalarSet(tmpPars[paramInds[j+1], ], 1 ,m, ids)
      } else{
        doVarList[[j]]$vectorSet(tmpPars[(paramInds[j]+1):paramInds[j+1], ], 1 ,m, ids)
      }
    }   
    if(isLast){
      for(i in 1:m){
        logWts[i] <-  mvWSamples['wts',i][currInd]
      }
      maxWt <- max(logWts)
      wts <- exp(logWts - maxWt)
      rankSample(wts, m, ids, silent)
      for(i in 1:m){
        copy(mvWSamples, mvEWSamples, thisXName, thisXName, ids[i], i)
      }
      for(j in 1:numParamVars){
        if(varSize[j] == 1){
          doVarList[[j]]$scalarSet(tmpPars[paramInds[j+1], ], 0 ,m, ids)
        } else{
          doVarList[[j]]$vectorSet(tmpPars[(paramInds[j]+1):paramInds[j+1], ], 0 ,m, ids)
        }
      } 
    }
    return(0)
  }
)

# Has two methods: shrinkMean, which shrinks each parameter particle towards
# the mean of all particles, and cholesVar, which returns the cholesky 
# decomposition of the weighted MC covariance matrix
LWparFunc <- nimbleFunction(
    name = 'LWparFunc',
  setup = function(d, parDim, prevInd){
    # Calculate h^2 and a using specified discount factor d
    hsq <- 1-((3*d-1)/(2*d))^2 
    a <- sqrt(1-hsq)
  },  
  methods = list(                        
    shrinkMean = function(m = integer(), logWts = double(1), pars = double(2)) {
      oneMat <- numeric(m, value = 1)
      maxWt <- max(logWts)
      wts <- exp(logWts - maxWt)  # don't modify logWts here as pass-by-reference in compiled code
      wts <- wts / sum(wts)
      parMean <- (pars%*%wts)%*%oneMat
      wtMean <- a*pars + (1-a)*parMean  
      returnType(double(2))
      return(wtMean)
    },
    cholesVar = function(m = integer(), logWts = double(1), pars = double(2)){
      varMat <- matrix(nrow = parDim, ncol = parDim, value = 0)
      returnType(double(2)) 
      maxWt <- max(logWts)  
      wts <- exp(logWts - maxWt)  # don't modify logWts here as pass-by-reference in compiled code
      wts <- wts / sum(wts)
      wMean <- pars%*%wts 
      for(i in 1:m)
        varMat <- varMat + wts[i]*((pars[,i]-wMean)%*%t(pars[,i]-wMean))
      varMat <- hsq*varMat
      if(length(varMat)==1)
        varMat[1,1] <- sqrt(varMat[1,1])
      else
        varMat <- chol(varMat)
      
      return(varMat)
    }
  )
)

#' Create a Liu and West particle filter algorithm.  
#' 
#' @description Create a Liu and West particle filter algorithm for a given NIMBLE state space model.  
#'
#' @param model A NIMBLE model object, typically representing a state 
#'  space model or a hidden Markov model
#' @param nodes  A character vector specifying the stochastic latent model nodes 
#'  over which the particle filter will stochastically integrate to
#'  estimate the log-likelihood function.  All provided nodes must be stochastic.
#'  Can be one of three forms: a variable name, in which case all elements in the variable
#'  are taken to be latent (e.g., 'x'); an indexed variable, in which case all indexed elements are taken
#'  to be latent (e.g., 'x[1:100]' or 'x[1:100, 1:2]'); or a vector of multiple nodes, one per time point,
#'  in increasing time order (e.g., c("x[1:2, 1]", "x[1:2, 2]", "x[1:2, 3]", "x[1:2, 4]")).
#' @param params A character vector specifying the top-level parameters to estimate the posterior distribution of. 
#'   If unspecified, parameter nodes are specified as all stochastic top level nodes which
#'  are not in the set of latent nodes specified in \code{nodes}.
#' @param control  A list specifying different control options for the particle filter.  Options are described in the details section below.

#' @author Nicholas Michaud
#' @family particle filtering methods
#' @details 
#' 
#' Each of the \code{control()} list options are described in detail below:
#' \describe{
#'  \item{d}{A discount factor for the Liu-West filter.  Should be close to,
#'  but not above, 1.}
#'  \item{saveAll}{Indicates whether to save state samples for all time points (TRUE), or only for the most recent time point (FALSE)}
#'  \item{timeIndex}{An integer used to manually specify which dimension of the latent state variable indexes time.  
#'  Only needs to be set if the number of time points is less than or equal to the size of the latent state at each time point.}
#'  \item{initModel}{A logical value indicating whether to initialize the model before running the filtering algorithm.  Defaults to TRUE.}
#' }
#' 
#'  The Liu and West filter samples from the posterior 
#'  distribution of both the latent states and top-level parameters for a state space model.  
#'  Each particle in the Liu and West filter contains values not only for latent states, 
#'  but also for top level parameters.  Latent states are propogated via an auxiliary step, 
#'  as in the auxiliary particle filter (\code{\link{buildAuxiliaryFilter}}).
#'  Top-level parameters are propagated from one 
#'  time point to the next through a smoothed kernel density based on previous particle values.  
#'        
#'  The resulting specialized particle filter algorthm will accept a
#'  single integer argument (\code{m}, default 10,000), which specifies the number
#'  of random \'particles\' to use for sampling from the posterior distributions.  The algorithm  saves
#'  unequally weighted samples from the posterior distribution of the latent
#'  states and top-level parameters in \code{mvWSamples}, with corresponding logged weights in \code{mvWSamples['wts',]}.
#'  An equally weighted sample from the posterior can be found in \code{mvEWSamples}. 
#'  
#'  Note that if \code{saveAll=TRUE}, the top-level parameter samples given in the \code{mvWSamples} output will correspond to the weights from the final time point.
#'
#' @export
#' 
#' @references Liu, J., and M. West. (2001). Combined parameter and state estimation in simulation-based filtering. \emph{Sequential Monte Carlo methods in practice}. Springer New York, pages 197-223.
#' 
#' @examples
#' ## For illustration only.
#' exampleCode <- nimbleCode({
#'   x0 ~ dnorm(0, var = 1)
#'   x[1] ~ dnorm(.8 * x0, var = 1)
#'   y[1] ~ dnorm(x[1], var = .5)
#'   for(t in 2:10){
#'     x[t] ~ dnorm(.8 * x[t-1], sd = sigma_x)
#'     y[t] ~ dnorm(x[t], var = .5)
#'   }
#'   sigma_x ~ dunif(0, 10)
#' })
#'
#' model <- nimbleModel(code = exampleCode, data = list(y = rnorm(10)),
#'                      inits = list(x0 = 0, x = rnorm(10), sigma_x = 1))
#' my_LWF <- buildLiuWestFilter(model, 'x', params = 'sigma_x')
#' ## Now compile and run, e.g.,
#' ## Cmodel <- compileNimble(model)
#' ## Cmy_LWF <- compileNimble(my_LWF, project = model)
#' ## Cmy_LWF$run(m = 1000)
#' ## lw_X <- as.matrix(Cmy_LWF$mvEWSamples, 'x')
#' ##  samples from posterior of top level parameter
#' ## lw_sigma_x <- as.matrix(Cmy_LWF$mvEWSamples, 'sigma_x')
buildLiuWestFilter <- nimbleFunction(
    name = 'buildLiuWestFilter',
  setup = function(model, nodes, params = NULL, control = list()){
    warning("The Liu-West filter ofen performs poorly and is provided primarily for didactic purposes.")
    #control list extraction
    saveAll <- control[['saveAll']]
    silent <- control[['silent']]
    d <- control[['d']]
    timeIndex <- control[['timeIndex']]
    initModel <- control[['initModel']]
    if(is.null(silent)) silent <- FALSE
    if(is.null(saveAll)) saveAll <- FALSE
    if(is.null(d)) d <- .99
    if(is.null(initModel)) initModel <- TRUE
    ## get latent state info
    nodes <- findLatentNodes(model, nodes, timeIndex)
    latentVars <- model$getVarNames(nodes = nodes)
    
    # if unspecified, parameter nodes are specified as all stochastic top level nodes which
    # are not in the set of latent nodes above
    if(all(is.null(params))){
      params <-  model$getNodeNames(stochOnly=TRUE, includeData=FALSE,
                                           topOnly=TRUE)
      parLatents <- sapply(params, function(x){return(model$getVarNames(nodes = x) %in% latentVars)})
      params <- params[!parLatents]
    }
    params <- model$expandNodeNames(params, sort = TRUE)
    if(identical(params, character(0)))
      stop('There must be at least one higher level parameter for Liu and West filter to work.')
    if(any(params %in% nodes))
      stop('Parameters cannot be latent states.')
    if(!all(params%in%model$getNodeNames(stochOnly=TRUE)))
      stop('Parameters must be stochastic nodes.')
    paramVars <-  model$getVarNames(nodes =  params)  # need var names too
    pardimcheck <- sapply(paramVars, function(n){
      if(length(nimDim(model[[n]]))>1)
        stop("Liu and West filter doesn't work for matrix valued top level parameters.")
    })
    
    dims <- lapply(nodes, function(n) nimDim(model[[n]]))
    if(length(unique(dims)) > 1) stop('Sizes or dimensions of latent states varies.')
    paramDims <-   sapply(params, function(n) nimDim(model[[n]]))
    
    my_initializeModel <- initializeModel(model, silent = silent)
    
    modelSymbolObjects = model$getSymbolTable()$getSymbolObjects()[c(latentVars, paramVars)]
    if(saveAll){
      
      names <- sapply(modelSymbolObjects, function(x)return(x$name))
      type <- sapply(modelSymbolObjects, function(x)return(x$type))
      size <- lapply(modelSymbolObjects, function(x){
        if(identical(x$size, numeric(0))) return(1)
        return(x$size)})      
      mvEWSamples <- modelValues(modelValuesConf(vars = names,
                                              types = type,
                                              sizes = size))
      
      
      names <- c(names, "wts")
      type <- c(type, "double")
      size$wts <- length(dims)
      
      mvWSamples  <- modelValues(modelValuesConf(vars = names,
                                              types = type,
                                              sizes = size))
      
    }
    else{
      names <- sapply(modelSymbolObjects, function(x)return(x$name))
      type <- sapply(modelSymbolObjects, function(x)return(x$type))
      size <- lapply(modelSymbolObjects, function(x){
        if(identical(x$size, numeric(0))) return(1)
        return(x$size)})
      
      size[[latentVars]] <- as.numeric(dims[[1]])      
      mvEWSamples <- modelValues(modelValuesConf(vars = names,
                                              types = type,
                                              sizes = size))
      
      names <- c(names, "wts")
      type <- c(type, "double")
      size$wts <- 1
      mvWSamples  <- modelValues(modelValuesConf(vars = names,
                                              types = type,
                                              sizes = size))
    }
    
    names <- names[1]
    varSymbolObjects = model$getSymbolTable()$getSymbolObjects()[paramVars]
    paramVarDims <- unlist(lapply(varSymbolObjects, function(x){
      if(identical(x$size, numeric(0))) return(1)
      return(x$size)}))
    #names <- model$getSymbolTable()$getSymbolObjects()[[latentVars]]$name
    LWStepFunctions <- nimbleFunctionList(LWStepVirtual)
    for(iNode in seq_along(nodes))
      LWStepFunctions[[iNode]] <- LWStep(model,  mvWSamples, mvEWSamples, nodes, paramVarDims, 
                                         iNode, params, paramVars, names, saveAll, d, silent)
    
  },
  run = function(m = integer(default = 10000)) {
    if(initModel == TRUE) my_initializeModel$run()
    resize(mvWSamples, m)
    resize(mvEWSamples, m)

    initWt <- -log(m)
    for(i in 1:m)
      mvWSamples['wts',i][1] <<- initWt
    
    for(iNode in seq_along(LWStepFunctions)) {
       LWStepFunctions[[iNode]]$run(m)
    }
    return()
  }
)


