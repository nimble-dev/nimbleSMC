#' Particle Filtering MCMC Sampling Algorithms
#' 
#' Details of the particle filtering MCMC sampling algorithms provided in nimbleSMC.
#'
#' @param model (uncompiled) model on which the MCMC is to be run
#' @param mvSaved \code{modelValues} object to be used to store MCMC samples
#' @param target node(s) on which the sampler will be used
#' @param control named list that controls the precise behavior of the sampler, with elements specific to \code{samplertype}.  The default values for control list are specified in the setup code of each sampling algorithm.  Descriptions of each sampling algorithm, and the possible customizations for each sampler (using the \code{control} argument) appear below.
#' 
#' @section RW_PF sampler:
#'
#' The particle filter sampler allows the user to perform particle MCMC (PMCMC) (Andrieu et al., 2010), primarily for state-space or hidden Markov models of time-series data. This method uses Metropolis-Hastings samplers for top-level parameters but uses the likelihood approximation of a particle filter (sequential Monte Carlo) to integrate over latent nodes in the time-series.  The \code{RW_PF} sampler uses an adaptive Metropolis-Hastings algorithm with a univariate normal proposal distribution for a scalar parameter.  Note that samples of the latent states can be retained as well, but the top-level parameter being sampled must be a scalar.   A bootstrap, auxiliary, or user defined particle filter can be used to integrate over latent states.
#'
#' For more information about user-defined samplers within a PMCMC sampler, see the NIMBLE User Manual.
#'
#' The \code{RW_PF} sampler accepts the following control list elements:
#' \itemize{
#' \item adaptive. A logical argument, specifying whether the sampler should adapt the scale (proposal standard deviation) throughout the course of MCMC execution to achieve a theoretically desirable acceptance rate. (default = TRUE)
#' \item adaptInterval. The interval on which to perform adaptation.  Every adaptInterval MCMC iterations (prior to thinning), the RW sampler will perform its adaptation procedure.  This updates the scale variable, based upon the sampler's achieved acceptance rate over the past adaptInterval iterations. (default = 200)
#' \item scale. The initial value of the normal proposal standard deviation.  If \code{adaptive = FALSE}, scale will never change. (default = 1)
#' \item pfNparticles.  The number of particles to use in the approximation to the log likelihood of the data (default = 1000).
#' \item latents.  Character vector specifying the nodes that are latent states over which the particle filter will operate to approximate the log-likelihood function.
#' \item pfType.  Character argument specifying the type of particle filter that should be used for likelihood approximation.  Choose from \code{"bootstrap"} and \code{"auxiliary"}.  Defaults to \code{"bootstrap"}.
#' \item pfControl.  A control list that is passed to the particle filter function.  For details on control lists for bootstrap or auxiliary particle filters, see \code{\link{buildBootstrapFilter}} or \code{\link{buildAuxiliaryFilter}} respectively.  Additionally, this can be used to pass custom arguments into a user-defined particle filter.
#' \item pfOptimizeNparticles.  A logical argument, specifying whether to use an experimental procedure to automatically determine the optimal number of particles to use, based on Pitt and Shephard (2011).  This will override any value of \code{pfNparticles} specified above.
#' \item pf.  A user-defined particle filter object, if a bootstrap or auxiliary particle filter is not adequate.
#' }
#' 
#' @section RW_PF_block sampler:
#'
#' The particle filter block sampler allows the user to perform particle MCMC (PMCMC) (Andrieu et al., 2010) for multiple parameters jointly, primarily for state-space or hidden Markov models of time-series data.  This method uses Metropolis-Hastings block samplers for top-level parameters but uses the likelihood approximation of a particle filter (sequential Monte Carlo) to integrate over latent nodes in the time-series.  The \code{RW_PF} sampler uses an adaptive Metropolis-Hastings algorithm with a multivariate normal proposal distribution.  Note that samples of the latent states can be retained as well, but the top-level parameter being sampled must be a scalar.   A bootstrap, auxiliary, or user defined particle filter can be used to integrate over latent states.
#'
#' For more information about user-defined samplers within a PMCMC sampler, see the NIMBLE User Manual.
#' 
#' The \code{RW_PF_block} sampler accepts the following control list elements:
#' \itemize{
#' \item adaptive. A logical argument, specifying whether the sampler should adapt the proposal covariance throughout the course of MCMC execution. (default = TRUE)
#' \item adaptScaleOnly. A logical argument, specifying whether adaptation should be done only for \code{scale} (TRUE) or also for \code{provCov} (FALSE).  This argument is only relevant when \code{adaptive = TRUE}.  When \code{adaptScaleOnly = FALSE}, both \code{scale} and \code{propCov} undergo adaptation; the sampler tunes the scaling to achieve a theoretically good acceptance rate, and the proposal covariance to mimic that of the empirical samples.  When \code{adaptScaleOnly = TRUE}, only the proposal scale is adapted. (default = FALSE)
#' \item adaptInterval. The interval on which to perform adaptation. (default = 200)
#' \item scale. The initial value of the scalar multiplier for \code{propCov}.  If \code{adaptive = FALSE}, \code{scale} will never change. (default = 1)
#' \item adaptFactorExponent. Exponent controling the rate of decay of the scale adaptation factor.  See Shaby and Wells, 2011, for details. (default = 0.8)
#' \item propCov. The initial covariance matrix for the multivariate normal proposal distribution.  This element may be equal to the \code{'identity'}, in which case the identity matrix of the appropriate dimension will be used for the initial proposal covariance matrix. (default is \code{'identity'})
#' \item pfNparticles.  The number of particles to use in the approximation to the log likelihood of the data (default = 1000).
#' \item latents.  Character vector specifying the nodes that are latent states over which the particle filter will operate to approximate the log-likelihood function.
#' \item pfType.  Character argument specifying the type of particle filter that should be used for likelihood approximation.  Choose from \code{"bootstrap"} and \code{"auxiliary"}.  Defaults to \code{"bootstrap"}.
#' \item pfControl.  A control list that is passed to the particle filter function.  For details on control lists for bootstrap or auxiliary particle filters, see \code{\link{buildBootstrapFilter}} or \code{\link{buildAuxiliaryFilter}} respectively.  Additionally, this can be used to pass custom arguments into a user defined particle filter.
#' \item pfOptimizeNparticles.  A logical argument, specifying whether to automatically determine the optimal number of particles to use, based on Pitt and Shephard (2011).  This will override any value of \code{pfNparticles} specified above.
#' \item pf.  A user-defined particle filter object, if a bootstrap or auxiliary particle filter is not adequate.
#' }
#' 
#' @name SMCsamplers
#' @aliases samplers sampler RW_PF RW_PF_block sampler_RW_PF sampler_RW_PF_block 
#' 
#' 




#######################################################################################
### RW_PF, does a univariate RW, but using a particle filter likelihood function ######
#######################################################################################
#' @rdname SMCsamplers
#' @export
sampler_RW_PF <- nimbleFunction(
    name = 'sampler_RW_PF',
    contains = sampler_BASE,
    setup = function(model, mvSaved, target, control) {
        ## control list extraction
        adaptive       <- if(!is.null(control$adaptive))             control$adaptive             else TRUE
        adaptInterval  <- if(!is.null(control$adaptInterval))        control$adaptInterval        else 200
        scale          <- if(!is.null(control$scale))                control$scale                else 1
        m              <- if(!is.null(control$pfNparticles))         control$pfNparticles         else 1000
        existingPF     <- if(!is.null(control$pf))                   control$pf                   else NULL
        filterType     <- if(!is.null(control$pfType))               control$pfType               else 'bootstrap'
        filterControl  <- if(!is.null(control$pfControl))            control$pfControl            else list()
        optimizeM      <- if(!is.null(control$pfOptimizeNparticles)) control$pfOptimizeNparticles else FALSE
        latents        <- if(!is.null(control$latents))              control$latents              else stop('RW_PF sampler missing required control argument: latents')
        
        if(!is.null(control$pfLookahead)) {
          print("Warning, the `pfLookahead` control list argument is deprecated
                and will not be supported in future versions of NIMBLE. Please
                specify the lookahead function via the pfControl argument 
                instead.")
          filterControl$lookahead  <-  control$pfLookahead
        }                    
        else if(is.null(filterControl$lookahead)) {
          filterControl$lookahead  <-  'simulate'
        } 
        
        ## node list generation
        targetAsScalar <- model$expandNodeNames(target, returnScalarComponents = TRUE)
        calcNodes <- model$getDependencies(target)
        latentSamp <- FALSE
        MCMCmonitors <- tryCatch(parent.frame(2)$conf$monitors, error = function(e) e)
        if(identical(MCMCmonitors, TRUE))
            latentSamp <- TRUE
        else if(any(model$expandNodeNames(latents) %in% model$expandNodeNames(MCMCmonitors)))
            latentSamp <- TRUE
        latentDep <- model$getDependencies(latents)
        topParams <- model$getNodeNames(stochOnly=TRUE, includeData=FALSE, topOnly=TRUE)
        ## numeric value generation
        optimizeM       <- as.integer(optimizeM)
        scaleOriginal   <- scale
        timesRan        <- 0
        timesAccepted   <- 0
        timesAdapted    <- 0
        prevLL          <- 0
        nVarEsts        <- 0
        itCount         <- 0
        optimalAR       <- 0.44
        gamma1          <- 0
        storeParticleLP <- -Inf
        storeLLVar      <- 0
        ## Number of LL estimates to compute to get each LL
        ## variance estimate for m optimization.
        nVarReps        <- 7   
        ## Number of LL variance estimates to compute before deciding optimal m.
        mBurnIn         <- 15   
        d               <- length(targetAsScalar)
        if(optimizeM) m <- 3000
        ## Nested function and function list definitions.
        my_setAndCalculate <- setAndCalculateOne(model, target)
        my_decideAndJump <- decideAndJump(model, mvSaved, calcNodes)
        if(!is.null(existingPF)) {
            my_particleFilter <- existingPF
        } else {
            if(latentSamp == TRUE) { 
                filterControl$saveAll <- TRUE
                filterControl$smoothing <- TRUE
            } else {
                filterControl$saveAll <- FALSE
                filterControl$smoothing <- FALSE
            }
            filterControl$initModel <- FALSE
            if(is.character(filterType) && filterType == 'auxiliary') {
                my_particleFilter <- buildAuxiliaryFilter(model, latents, 
                                                          control = filterControl)
            }
            else if(is.character(filterType) && filterType == 'bootstrap') {
                my_particleFilter <- buildBootstrapFilter(model, latents,
                                                          control = filterControl)
            }
            else if(is.nfGenerator(filterType)){
                my_particleFilter <- filterType(model, latents,
                                                control = filterControl)
            }
            else stop('filter type must be either "bootstrap", "auxiliary", or a
                  user defined filtering algorithm created by a call to 
                  nimbleFunction(...).')
        }
        particleMV <- my_particleFilter$mvEWSamples
        ## checks
        if(any(target%in%model$expandNodeNames(latents)))   stop('PMCMC \'target\' argument cannot include latent states')
        if(length(targetAsScalar) > 1)                      stop('more than one top-level target; cannot use RW_PF sampler, try RW_PF_block sampler')
    },
    run = function() {
        storeParticleLP <<- my_particleFilter$getLastLogLik()
        modelLP0 <- storeParticleLP + getLogProb(model, target)
        propValue <- rnorm(1, mean = model[[target]], sd = scale)
        my_setAndCalculate$run(propValue)
        particleLP <- my_particleFilter$run(m)
        modelLP1 <- particleLP + getLogProb(model, target)
        jump <- my_decideAndJump$run(modelLP1, modelLP0, 0, 0)
        if(!jump) {
            my_particleFilter$setLastLogLik(storeParticleLP)
        }
        if(jump & latentSamp){
            ## if we jump, randomly sample latent nodes from pf output and put into model so that they can be monitored
            index <- ceiling(runif(1, 0, m))
            copy(particleMV, model, latents, latents, index)
            calculate(model, latentDep)
            copy(from = model, to = mvSaved, nodes = latentDep, row = 1, logProb = TRUE)
        }
        else if(!jump & latentSamp){
            ## if we don't jump, replace model latent nodes with saved latent nodes
            copy(from = mvSaved, to = model, nodes = latentDep, row = 1, logProb = TRUE)
        }
##        if(jump & !resample)  storeParticleLP <<- particleLP
        if(jump & optimizeM) optimM()
        if(adaptive)     adaptiveProcedure(jump)
    },
    methods = list(
        optimM = function() {
            tempM <- 15000
            declare(LLEst, double(1, nVarReps))
            if(nVarEsts < mBurnIn) {  # checks whether we have enough var estimates to get good approximation
                for(i in 1:nVarReps)
                    LLEst[i] <- my_particleFilter$run(tempM)
                ## next, store average of var estimates
                if(nVarEsts == 1)
                    storeLLVar <<- var(LLEst)/mBurnIn
                else {
                    LLVar <- storeLLVar
                    LLVar <- LLVar + var(LLEst)/mBurnIn
                    storeLLVar<<- LLVar
                }
                nVarEsts <<- nVarEsts + 1
            }
            else {  # once enough var estimates have been taken, use their average to compute m
                m <<- m*storeLLVar/(0.92^2)
                m <<- ceiling(m)
                storeParticleLP <<- my_particleFilter$run(m)
                optimizeM <<- 0
            }
        },
        adaptiveProcedure = function(jump = logical()) {
            timesRan <<- timesRan + 1
            if(jump)     timesAccepted <<- timesAccepted + 1
            if(timesRan %% adaptInterval == 0) {
                acceptanceRate <- timesAccepted / timesRan
                timesAdapted <<- timesAdapted + 1
                gamma1 <<- 1/((timesAdapted + 3)^0.8)
                gamma2 <- 10 * gamma1
                adaptFactor <- exp(gamma2 * (acceptanceRate - optimalAR))
                scale <<- scale * adaptFactor
                timesRan <<- 0
                timesAccepted <<- 0
            }
        },
        reset = function() {
            scale <<- scaleOriginal
            timesRan      <<- 0
            timesAccepted <<- 0
            timesAdapted  <<- 0
            storeParticleLP <<- -Inf
            gamma1 <<- 0
        }
    )
)



#######################################################################################
### RW_PF_block, does a block RW, but using a particle filter likelihood function #####
#######################################################################################

#' @rdname SMCsamplers
#' @export
sampler_RW_PF_block <- nimbleFunction(
    name = 'sampler_RW_PF_block',
    contains = sampler_BASE,
    setup = function(model, mvSaved, target,  control) {
        ## control list extraction
        adaptive            <- if(!is.null(control$adaptive))             control$adaptive             else TRUE
        adaptScaleOnly      <- if(!is.null(control$adaptScaleOnly))       control$adaptScaleOnly       else FALSE
        adaptInterval       <- if(!is.null(control$adaptInterval))        control$adaptInterval        else 200
        adaptFactorExponent <- if(!is.null(control$adaptFactorExponent))  control$adaptFactorExponent  else 0.8
        scale               <- if(!is.null(control$scale))                control$scale                else 1
        propCov             <- if(!is.null(control$propCov))              control$propCov              else 'identity'
        existingPF          <- if(!is.null(control$pf))                   control$pf                   else NULL
        m                   <- if(!is.null(control$pfNparticles))         control$pfNparticles         else 1000
        filterType          <- if(!is.null(control$pfType))               control$pfType               else 'bootstrap'
        filterControl       <- if(!is.null(control$pfControl))            control$pfControl            else list()
        optimizeM           <- if(!is.null(control$pfOptimizeNparticles)) control$pfOptimizeNparticles else FALSE
        latents             <- if(!is.null(control$latents))              control$latents              else stop('RW_PF sampler missing required control argument: latents')
        
        if(!is.null(control$pfLookahead)) {
          print("Warning, the `pfLookahead` control list argument is deprecated
                and will not be supported in future versions of NIMBLE. Please
                specify the lookahead function via the pfControl argument 
                instead.")
          filterControl$lookahead  <-  control$pfLookahead
        }                    
        else if(is.null(filterControl$lookahead)) {
          filterControl$lookahead  <-  'simulate'
        } 
        
        ## node list generation
        targetAsScalar <- model$expandNodeNames(target, returnScalarComponents = TRUE)
        calcNodes <- model$getDependencies(target)
        latentSamp <- FALSE
        MCMCmonitors <- tryCatch(parent.frame(2)$conf$monitors, error = function(e) e)
        if(identical(MCMCmonitors, TRUE))
            latentSamp <- TRUE
        else if(any(model$expandNodeNames(latents) %in% model$expandNodeNames(MCMCmonitors)))
            latentSamp <- TRUE
        latentDep <- model$getDependencies(latents)
        topParams <- model$getNodeNames(stochOnly=TRUE, includeData=FALSE, topOnly=TRUE)
        target <- model$expandNodeNames(target)
        ## numeric value generation
        optimizeM     <- as.integer(optimizeM)
        scaleOriginal <- scale
        timesRan      <- 0
        timesAccepted <- 0
        timesAdapted  <- 0
        prevLL        <- 0
        nVarEsts      <- 0
        itCount       <- 0
        d <- length(targetAsScalar)
        if(is.character(propCov) && propCov == 'identity')     propCov <- diag(d)
        propCovOriginal <- propCov
        chol_propCov <- chol(propCov)
        chol_propCov_scale <- scale * chol_propCov
        empirSamp <- matrix(0, nrow=adaptInterval, ncol=d)
        storeParticleLP <- -Inf
        storeLLVar  <- 0
        nVarReps <- 7    # number of LL estimates to compute to get each LL variance estimate for m optimization
        mBurnIn  <- 15   # number of LL variance estimates to compute before deciding optimal m
        if(optimizeM)   m <- 3000
        ## nested function and function list definitions
        my_setAndCalculate <- setAndCalculate(model, target)
        my_decideAndJump <- decideAndJump(model, mvSaved, calcNodes)
        my_calcAdaptationFactor <- nimble:::calcAdaptationFactor(d, adaptFactorExponent)
        if(!is.null(existingPF)) {
            my_particleFilter <- existingPF
        } else {
            if(latentSamp == TRUE) { 
                filterControl$saveAll <- TRUE
                filterControl$smoothing <- TRUE
            } else {
                filterControl$saveAll <- FALSE
                filterControl$smoothing <- FALSE
            }
            filterControl$initModel <- FALSE
            if(is.character(filterType) && filterType == 'auxiliary') {
                my_particleFilter <- buildAuxiliaryFilter(model, latents, 
                                                          control = filterControl)
            }
            else if(is.character(filterType) && filterType == 'bootstrap') {
                my_particleFilter <- buildBootstrapFilter(model, latents,
                                                          control = filterControl)
            }
            else if(is.nfGenerator(filterType)){
                my_particleFilter <- filterType(model, latents,
                                                control = filterControl)
                
            }
            else stop('filter type must be either "bootstrap", "auxiliary", or a
                  user defined filtering algorithm created by a call to 
                  nimbleFunction(...).')
        }
        particleMV <- my_particleFilter$mvEWSamples
        ## checks
        if(!inherits(propCov, 'matrix'))        stop('propCov must be a matrix\n')
        if(!inherits(propCov[1,1], 'numeric'))  stop('propCov matrix must be numeric\n')
        if(!all(dim(propCov) == d))           stop('propCov matrix must have dimension ', d, 'x', d, '\n')
        if(!isSymmetric(propCov))             stop('propCov matrix must be symmetric')
        if(length(targetAsScalar) < 2)        stop('less than two top-level targets; cannot use RW_PF_block sampler, try RW_PF sampler')
        if(any(target%in%model$expandNodeNames(latents)))   stop('PMCMC \'target\' argument cannot include latent states')
    },
    run = function() {
        storeParticleLP <<- my_particleFilter$getLastLogLik()
        modelLP0 <- storeParticleLP + getLogProb(model, target)
        propValueVector <- generateProposalVector()
        my_setAndCalculate$run(propValueVector)
        particleLP <- my_particleFilter$run(m)
        modelLP1 <- particleLP + getLogProb(model, target)
        jump <- my_decideAndJump$run(modelLP1, modelLP0, 0, 0)
        if(!jump) {
            my_particleFilter$setLastLogLik(storeParticleLP)
        }
        if(jump & latentSamp) {
            ## if we jump, randomly sample latent nodes from pf output and put
            ## into model so that they can be monitored
            index <- ceiling(runif(1, 0, m))
            copy(particleMV, model, latents, latents, index)
            calculate(model, latentDep)
            copy(from = model, to = mvSaved, nodes = latentDep, row = 1, logProb = TRUE)
        }
        else if(!jump & latentSamp) {
            ## if we don't jump, replace model latent nodes with saved latent nodes
            copy(from = mvSaved, to = model, nodes = latentDep, row = 1, logProb = TRUE)
        }
      ##  if(jump & !resample)  storeParticleLP <<- particleLP
        if(jump & optimizeM) optimM()
        if(adaptive)     adaptiveProcedure(jump)
    },
    methods = list(
        optimM = function() {
            tempM <- 15000
            declare(LLEst, double(1, nVarReps))
            if(nVarEsts < mBurnIn) {  # checks whether we have enough var estimates to get good approximation
                for(i in 1:nVarReps)
                    LLEst[i] <- my_particleFilter$run(tempM)
                ## next, store average of var estimates
                if(nVarEsts == 1)
                    storeLLVar <<- var(LLEst)/mBurnIn
                else {
                    LLVar <- storeLLVar
                    LLVar <- LLVar + var(LLEst)/mBurnIn
                    storeLLVar <<- LLVar
                }
                nVarEsts <<- nVarEsts + 1
            }
            else {  # once enough var estimates have been taken, use their average to compute m
                m <<- m*storeLLVar/(0.92^2)
                m <<- ceiling(m)
                storeParticleLP <<- my_particleFilter$run(m)
                optimizeM <<- 0
            }
        },
        generateProposalVector = function() {
            propValueVector <- rmnorm_chol(1, values(model,target), chol_propCov_scale, 0)  ## last argument specifies prec_param = FALSE
            returnType(double(1))
            return(propValueVector)
        },
        adaptiveProcedure = function(jump = logical()) {
            timesRan <<- timesRan + 1
            if(jump)     timesAccepted <<- timesAccepted + 1
            if(!adaptScaleOnly)     empirSamp[timesRan, 1:d] <<- values(model, target)
            if(timesRan %% adaptInterval == 0) {
                acceptanceRate <- timesAccepted / timesRan
                timesAdapted <<- timesAdapted + 1
                adaptFactor <- my_calcAdaptationFactor$run(acceptanceRate)
                scale <<- scale * adaptFactor
                ## calculate empirical covariance, and adapt proposal covariance
                if(!adaptScaleOnly) {
                    gamma1 <- my_calcAdaptationFactor$getGamma1()
                    for(i in 1:d)     empirSamp[, i] <<- empirSamp[, i] - mean(empirSamp[, i])
                    empirCov <- (t(empirSamp) %*% empirSamp) / (timesRan-1)
                    propCov <<- propCov + gamma1 * (empirCov - propCov)
                    chol_propCov <<- chol(propCov)
                }
                chol_propCov_scale <<- chol_propCov * scale
                timesRan <<- 0
                timesAccepted <<- 0
            }
        },
        reset = function() {
            scale   <<- scaleOriginal
            propCov <<- propCovOriginal
            chol_propCov <<- chol(propCov)
            chol_propCov_scale <<- chol_propCov * scale
            storeParticleLP <<- -Inf
            timesRan      <<- 0
            timesAccepted <<- 0
            timesAdapted  <<- 0
            my_calcAdaptationFactor$reset()
        }
    )
)
