#-----------------------------------------------------
#--- combined pipeline
#-----------------------------------------------------

estimate <- .         %>%
  stanify_input       %>%
  call_stan           %>%
  check_convergence   %>%
  extract_projections

#-----------------------------------------------------
#--- create STAN input
#-----------------------------------------------------

stanify_input <- function(x, simple = FALSE) {
  x %>%
  tidyr::complete(gender, level, tidyr::nesting(region, country), year,
                    fill = list(population = 0,
                                attainers  = 0,
                                status     = 'proj')) 				%>%
    arrange(year, country, level, gender) 					%>%
    as.list 												%>%
    within(., {
      #
      # dimensions
      N_cntr      	<- n_distinct(country)
      N_lvl       	<- n_distinct(level)
      N_gndr      	<- n_distinct(gender)
      N_yrs_obs  		<- n_distinct(year)
      N_regs      	<- n_distinct(region)
      ddim 			<- c(N_gndr, N_lvl, N_cntr, N_yrs_obs)
      #
      # time-related
      N_yrs_tru 		<- 30
      year_tru 		<- 0:29
      year_obs    	<-  ((year - min(year)) %/% 5) %>%
        unique 					  %>%
        sort
      shift_yr    	<- pmax(0, year_tru - 12)
      shift_year_tru 	<- sapply(4:0, function(i) pmax(0, i + year_tru - 12))[, c(2,3,3,4,5)]
      shift_year_obs 	<- shift_year_tru[year_obs + 1, ]
      #
      # covergence parameters
      conv_factor_tru <- apply(shift_year_tru, 2, function(r) cumsum((pmin(6, r))/6))
      conv_factor_obs <- conv_factor_tru[year_obs + 1,]
      nu_factor 		<- apply(shift_year_tru, 2, function(r) 0.5 * pmin(r, 2) + 1)
      nu_factor_obs 	<- nu_factor[year_obs + 1,]
      #
      # data
      regs        	<- as.integer(region[match(unique(country), country)])
      hasTarget   	<- tapply(status, level,
                             function(x) max(x == 'trgt'))
      obs_idx			<- as.numeric(status == 'obsv') %>%
        array(ddim) %>%
        {which(. == 1, arr.ind = TRUE)}
      N_obs 			<- nrow(obs_idx)
      tgt_idx 		<- as.numeric(status == 'trgt') %>%
        array(ddim) %>%
        {which(. == 1, arr.ind = TRUE)}
      N_tgt 			<- nrow(tgt_idx)
      bin_idx 		<- rbind(obs_idx, tgt_idx)
      N_bin 			<- nrow(bin_idx)
      # pseudopop   	<- array(population, ddim)
      pseudoatt   	<- {pmax(50, pmin(9950, attainers))/popN} %>%
        qnorm %>%
        array(ddim)
      obs  			<- pseudoatt[as.matrix(obs_idx)]
      tgt 			<- pseudoatt[as.matrix(tgt_idx)]

      hasTarget_idx 			<- match(1, hasTarget, NA)

      hasTargetRightBelow 	<- lead(hasTarget, default = 0)
      hasTargetRightBelow_idx <- max(0, which(hasTargetRightBelow > 0))
      x_hasTargetRightBelow 	<- min(1, hasTargetRightBelow_idx)

      hasTarget2Below 		<- lead(hasTargetRightBelow, default = 0)
      hasTarget2Below_idx 	<- max(0, which(hasTarget2Below > 0))
      x_hasTarget2Below 		<- min(1, hasTarget2Below_idx)

      hasTargetAbove_idx 		<- (cummax(hasTarget) > 0) %>%
        which 				   %>%
        setdiff(hasTarget_idx)
      N_hasTargetAbove 		<- length(hasTargetAbove_idx)

      hasNilTarget_idx 		<- union(hasTarget_idx,
                                  hasTargetAbove_idx) 	  %>%
        union(hasTargetRightBelow_idx) %>%
        union(hasTarget2Below_idx) 	  %>%
        setdiff(1:N_lvl, .) 			  %>%
        array(., dim = length(.))
      N_hasNilTarget 			<- length(hasNilTarget_idx)
      #
      # switches
      w_spillover 	   <- 1
      w_mean_reversion <- if (simple) 0 else 1
      w_xcntr_conv 	   <- 1
      w_gndr_conv 	   <- if (simple) 0 else 1
      w_target 		 <- max(hasTarget)
    }) 														%>%
    rlist::list.remove(c('gender', 'level', 'region', 'country', 'ddim',
                  'year', 'population', 'attainers', 'status',
                  'pseudoatt'))
}

#-----------------------------------------------------
#--- call STAN
#-----------------------------------------------------

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

call_stan <- function(x) {
  if (!edprojOPTS('testing')) {
  rstan::stan(data = x,
              model_code = stan_model,
              # file = 'model.stan',
              # sample_file = 'samples',
              # diagnostic_file = 'diagnostics',
              seed = 1020,
              chains = 4,
              iter = 250,
              init_r = 1,
              refresh = 10,
              control = list(max_treedepth = 12,
                             adapt_delta   = .65))
  } else {
    rstan::stan(data = x,
                model_code = stan_model,
                seed = 1020,
                chains = 1,
                iter = 25,
                init_r = 1,
                refresh = 10,
                control = list(max_treedepth = 12,
                               adapt_delta   = .65))
  }
}

#-----------------------------------------------------
#--- diagnostics
#-----------------------------------------------------

check_convergence <- function(x) {
  if (!edprojOPTS('testing')) {
  print("Checking convergence...")
  rstan_diagnostic_summary <- summary(x)$summary
  cat(rstan_diagnostic_summary, file = 'rstan_diagnostic_summary.txt')
  convergence <- all(sapply(rstan_diagnostic_summary[, 'Rhat'], function(x) {is.finite(x) & x <= 1.1}))
  if (!convergence)
    {print("Non-convergence in at least one MCMC chain")
     return(NA)}
  else {return(x)}
  } else {return(x)}
}

#-----------------------------------------------------
#--- extract results
#-----------------------------------------------------

extract_helper <- function(x, parStr)
{
  rstan::extract(x, parStr)[[parStr]] %>%
  {reshape2::melt(.)} 											           %>%
  setNames(c('iteration', 'gender',
              'level', 'country',
              'year', 'value'))		     %>%
  within(., {
    year    <- as.numeric(as.character(year)) * 5 + 1965
    country <- data_countries$country[country]
    gender  <- factor(gender, levels = 1:2, labels = c('female', 'male'))
    level   <- ordered(level, levels = 1:5, labels = rev(c('someP', 'P', 'loS', 'upS', 'postS')))
  }) %>%
  left_join(data_countries, by = 'country')
}

extract_projections <- function(x)
{
  bind_rows(
    TRD = extract_helper(x, 'trajectory_trend'),
    TGT = extract_helper(x, 'trajectory_target'),
    .id = 'scenario'
  )
}
