
benchmark_all <- function(){

# -----------------------------------------------------------------------------

# Schaake OR (Window / climate) Schaake OR SimSchaake (not written yet)

# schaake_shuffle() is the function that currently does the shuffling
# need to pass a template of observations to this function

# Internal functions that get called in schaake_shuffle() are in
# shuffle_members_internal.R and inlcude rank_members() and sort_members()
# Seems I dropped order_members() function in this file
# and use the newer reorder_members()

# sample_schaake_dates() window of dates that are similar to the forecast
# there is some intelligent handling here of missing dates

# once dates are selected a user can then use schaake_shuffle()

# No code for SimSchaake currently

# Code to get minimum divergence dates is explained in schaake_template_mindiv()
# (this needs to be optional in benchmark all as its slow)

# once dates from min div are gotten again can just use schaake_shuffle()

# -----------------------------------------------------------------------------

# ECC

# To run ECC we need to pass through a quantile sampling method (letter)
# A function and its parameters to sample from
# Then the raw forecast as the template

# There is the function get_ecc_quantiles() to handle quantile sampling
# This helps code the different ECC sub methods based one sampling
# eg R - random, Q - equi-spaced quantils , S - jittered

# The function sample_ecc_members() then generates the post-processed
#  ensemble members (works for all inbuild distribution I think but not ecdf ? )

# This apply_ecc_template() performs the shuffle relative to the raw forecast
# This function could be generalised

# Internal functions that get called in apply_ecc_template() are the same as
# those used in the schaake_shuffle()

# apply_ecc_template() and schaake_shuffle() could be replaced with one shuffle()
# function

# -----------------------------------------------------------------------------

# Need to think about what we want to return from here

# eg. Return post-processed forecasts

# Scoring the forecasts should be separate ??

}
