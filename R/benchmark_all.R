
benchmark_all <- function(){

# Schaake

# (Window / climate) Schaake

# SimSchaake

# -----------------------------------------------------------------------------

# MinDiv (this needs to be optional as its slow)
# Wrote the details fo this out in schaake_template_mindiv()

# -----------------------------------------------------------------------------

# ECC

# There is the function get_ecc_quantiles() to handle quantile sampling
# This helps code the different ECC sub methods based one sampling
# eg R - random, Q - equi-spaced quantils , S - jittered

# The function sample_ecc_members() then generates the post-processed
#  ensemble members (works for all inbuild distribution I think but not ecdf ? )

# This apply_ecc_template() performs the shuffle relative to the raw forecast
# This function could be generalised

# Internal functions that get called in apply_ecc_template() are in
# shuffle_members_internal.R and inlcude rank_members() and sort_members()
# Seems I dropped order_members() function in this file
# and use the newer reorder_members() that is also used in Schaake

# -----------------------------------------------------------------------------

# Need to think about what we want to return from here

# eg. Return post-processed forecasts

# Scoring the forecasts should be separate ??

}
