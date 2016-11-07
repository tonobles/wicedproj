stan_model <- "
	/*
	 * Wittgenstein Centre for Demography and Global Human Capital
*
* Multi-Level Educational Attainment Expansion Model
* --------------------------------------------------
* Copyright: Bilal Barakat <bilal.barakat@oeaw.ac.at>
* Date: 1 November 2015
* License: Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)
*
*/

functions{
}

data {
// SWITCHES
int w_target;
int w_mean_reversion; // mean reversion
int w_gndr_conv;      // gender convergence
int w_xcntr_conv;     // x-country convergence
int w_spillover;      // expansion pressure upwards

// DIMENSIONS
int N_yrs_tru; // # years in observed + projection horizon
int N_yrs_obs; // # years observed + 'observed' targets
int N_cntr;	   // # of countries
int N_lvl;     // # of education levels
int N_gndr;    // # of genders = 2
int N_regs;    // # of regions
int N_bin;     // length of vector of observed values
int N_tgt;
int N_obs;

// TIME DIMENSION
vector[N_yrs_tru]       year_tru;		    // all years
matrix[N_yrs_tru,N_lvl] shift_year_tru; // all years relative to trend break
matrix[N_yrs_tru,N_lvl] conv_factor_tru;
vector[N_yrs_obs]       year_obs;	      // observed years
matrix[N_yrs_obs,N_lvl] shift_year_obs; // obs yrs relative to trend break
matrix[N_yrs_obs,N_lvl] conv_factor_obs;
matrix[N_yrs_tru,N_lvl] nu_factor;
matrix[N_yrs_obs,N_lvl] nu_factor_obs;

// TARGETS B/W LEVELS
int hasTarget_idx;
int x_hasTargetRightBelow;
int hasTargetRightBelow_idx;
int x_hasTarget2Below;
int hasTarget2Below_idx;
int N_hasTargetAbove;
int hasTargetAbove_idx[N_hasTargetAbove];
int N_hasNilTarget;
int hasNilTarget_idx[N_hasNilTarget];

// OBSERVATIONS
// in two parts because indices must be integers
// and matrices/arrays cannot be of mixed type
int bin_idx[N_bin,4];	// parameters of observed values: gender, level, country, year
int tgt_idx[N_tgt,4];
int regs[N_cntr];
vector[N_obs] obs;	    // observed values
vector[N_tgt] tgt;
}

parameters {
// EFFECTS
matrix<lower=-4,upper=4>[N_cntr,N_lvl] initial_lvl[N_gndr];                  // intercepts
matrix<lower= 0,upper=3>[N_gndr,N_cntr] trend_shift[N_lvl]; // to reach target
matrix<lower= 0,upper=0.5>[N_lvl,N_cntr] nu;
matrix[N_cntr,N_lvl] trend[N_gndr];												 // slopes
matrix<lower=0>[N_regs,N_lvl] rtrend[N_gndr];
// ERRORS
matrix[N_cntr,N_yrs_tru] eps[N_gndr,N_lvl]; // random shocks
real<lower=0,upper=1> theta;  // strength of mean reversion
// VARIANCES
real<lower=0> trend_sigma[N_lvl];
real<lower=0> sigma[N_lvl];
}

transformed parameters {
}

model {
matrix[N_cntr,N_yrs_tru] cum_eps[N_gndr,N_lvl];
vector[N_bin] probs;

to_vector(nu)				~ beta(1, 5);
theta						~ beta(1.5, 1.5);
trend_sigma  				~ normal(0, 0.2);
sigma						~ normal(0, 0.2);

for (g in 1:N_gndr)	{
to_vector(initial_lvl[g]) ~ uniform(-4, 4);
to_vector(rtrend[g])      ~ normal(0, 1);
to_vector(trend[g])       ~ normal(0, 1);
for (e in 1:N_lvl) {
to_vector(eps[g,e])     ~ normal(0, 1);
for (c in 1:N_cntr) {
cum_eps[g,e,c] = cumulative_sum(eps[g,e,c]);
}
}
}

to_vector(trend_shift[hasTarget_idx]) 			 ~ exponential(0.5);
for (i in 1:N_hasTargetAbove)
to_vector(trend_shift[hasTargetAbove_idx[i]])   ~ normal(to_vector(trend_shift[hasTarget_idx]), 0.001);
for (i in 1:N_hasNilTarget)
to_vector(trend_shift[hasNilTarget_idx[i]])     ~ normal(0, 0.001);
if (x_hasTargetRightBelow) {
if (w_spillover)
to_vector(trend_shift[hasTargetRightBelow_idx]) ~ normal(0.5 * to_vector(trend_shift[hasTarget_idx]), 0.001);
else
to_vector(trend_shift[hasTargetRightBelow_idx]) ~ normal(0, 0.001);
}
if (x_hasTarget2Below) {
if (w_spillover)
to_vector(trend_shift[hasTarget2Below_idx])     ~ normal(0.1 * to_vector(trend_shift[hasTarget_idx]), 0.001);
else
to_vector(trend_shift[hasTarget2Below_idx])     ~ normal(0, 0.001);
}

// likelihood
for (i in 1:N_bin) {
int gg;
int ee;
int cc;
int tt;
int rr;
gg = bin_idx[i,1];
ee = bin_idx[i,2];
cc = bin_idx[i,3];
tt = bin_idx[i,4];
rr = regs[cc];
probs[i] =
initial_lvl[gg,cc,ee]
+ rtrend[gg,rr,ee] * year_obs[tt]
+ trend[gg,cc,ee] * year_obs[tt] * trend_sigma[ee]
+ (cum_eps[gg,ee,cc,tt] - eps[gg,ee,cc,1]) * sigma[ee];

if (w_mean_reversion && tt > 1)
probs[i] = probs[i]
- cum_eps[gg,ee,cc, tt - 1] * sigma[ee] * theta;

if (w_xcntr_conv)
probs[i] = probs[i]
- trend[gg,cc,ee] * conv_factor_obs[tt,ee] * trend_sigma[ee]
;

if (gg == 2) {
int f;
int m;
f = i;
m = i - 1;
if (w_gndr_conv) {
real average;
real nnu;
nnu = fmin(0.5, nu[ee,cc] * nu_factor_obs[tt,ee]);
average = mean(segment(probs, m, 2));
probs[m] = (1 - nnu) * probs[m] + nnu * average;
probs[f] = (1 - nnu) * probs[f] + nnu * average;
}
if (w_target) {
probs[f] = probs[f] + trend_shift[ee,2,cc] * shift_year_obs[tt,ee];
probs[m] = probs[m] + trend_shift[ee,1,cc] * shift_year_obs[tt,ee];
}
}
}

obs ~ normal(head(probs, N_obs), 0.05);
# tgt ~ normal(tail(probs, N_tgt), 0.05);
-tgt ~ exp_mod_normal(-tail(probs, N_tgt), 0.05, 0.5);
}

generated quantities {
matrix[N_cntr,N_yrs_tru] trajectory_trend[N_gndr,N_lvl];
matrix[N_cntr,N_yrs_tru] trajectory_target[N_gndr,N_lvl];
matrix[N_cntr,N_yrs_tru] cum_eps[N_gndr,N_lvl];
vector[N_cntr] slope_trend[N_gndr,N_lvl];
vector[N_cntr] slope_target[N_gndr,N_lvl];

for (g in 1:N_gndr) {
for (e in 1:N_lvl) {
for (c in 1:N_cntr) {
int r;
r = regs[c];
cum_eps[g,e,c]     = cumulative_sum(eps[g,e,c]);
trajectory_trend[g,e,c,1]  = initial_lvl[g,c,e];
slope_trend[g,e,c] = rtrend[g,r,e] + trend[g,c,e] * trend_sigma[e];
slope_target[g,e,c] = slope_trend[g,e,c] + trend_shift[e,g,c];
for (t in 2:N_yrs_tru) {
trajectory_trend[g,e,c,t] =
initial_lvl[g,c,e]
+ rtrend[g,r,e] * year_tru[t]
+ trend[g,c,e] * year_tru[t] * trend_sigma[e]
+ (cum_eps[g,e,c,t] - eps[g,e,c,1]) * sigma[e];
if (w_mean_reversion)
trajectory_trend[g,e,c,t] = trajectory_trend[g,e,c,t]
- cum_eps[g,e,c,t-1] * sigma[e] * theta;
if (w_xcntr_conv)
trajectory_trend[g,e,c,t] = trajectory_trend[g,e,c,t]
- trend[g,c,e] * conv_factor_tru[t,e] * trend_sigma[e]
;
}
}
}
}

for (e in 1:N_lvl) {
for (c in 1:N_cntr) {
for (t in 1:N_yrs_tru) {
if (w_gndr_conv) {
real average_trend;
real nnu;
nnu = nu[e,c] * nu_factor[t,e];
average_trend = (trajectory_trend[1,e,c,t] + trajectory_trend[2,e,c,t])/2;
trajectory_trend[1,e,c,t] =
(1 - nnu) * trajectory_trend[1,e,c,t] + nnu * average_trend;
trajectory_trend[2,e,c,t] =
(1 - nnu) * trajectory_trend[2,e,c,t] + nnu * average_trend;
}
for (g in 1:N_gndr) {
trajectory_target[g,e,c,t] =
Phi(trajectory_trend[g,e,c,t]	+ trend_shift[e,g,c] * shift_year_tru[t,e]);
trajectory_trend[g,e,c,t]  =
Phi(trajectory_trend[g,e,c,t]);
}
}
}
}


}

"
