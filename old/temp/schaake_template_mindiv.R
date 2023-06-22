schaake_template_mindiv <- function(){

  # I'm not sure what this needs...
  # existing functions:
  #   min_div_thin_dates
  #   min_div_stepwise_backward_selection
  #   min_div_single_backward_selection
  #   get_min_div_value
  #   get_min_div_total
  #   get_min_div_epsilon


  # (Optional) min_div_thin_dates
  # Min div is computationally expensive.
  # This function is an optional function that can be used first to
  # reduce the number of dates used to calculate the minimum divergence
  # Thinning basically based on which observations are the most similar

  # The next call is to either min_div_stepwise_backward_selection or
  # min_div_single_backward_selection

  # Again as the computation is so expensive, in choosing the dates to use as the
  # template Schuer uses backward selection.
  # Instead of doing this one at a time (eg single) you can make larger jumps in
  # the search to find the set of dates that minimised the divergence
  # Looking back here - there is no reason that these need to be separate functions
  # Although I do see that I set up the mindiv single backward selection using
  # clusters to speed things up

  # get_min_div_epsilon is an internal function called by get_minimum_divergence
  # (note genius past Kate called the file get_min_div_value.R that contains
  # get_minimum_divergence() - normally they are the same

  # I can also see here I have done something weird with semi-duplicated functions
  # get_min_div_total and get_minimum_divergence seem to be broadly similar ??
  # Maybe one is vector and on is a matrix generalisation ??
  # I'll have to take a closer look

  # I think this catalogues all the functionality here

  # Beaware this always ran deathly slow - and I think the actually estimation
  # of the minimum divergence should ideally be passed out to C to speed this up
}
