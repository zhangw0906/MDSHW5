get_jordan_pts <- function(df , year = 1986) {
  
  # read the data
  bulk_data = df
  # select the year.
  filter(bulk_data , Year == year)  %>%
    select(Player, PTS) %>%
    filter(Player == 'Michael Jordan' | Player == 'Michael Jordan*' ) %>%
    select(PTS) -> jordan_pts
  
  if (nrow(jordan_pts) == 0) {
    print(" Jordan did not play this year")
    return(0)
  }
  else {
    pt_val <- unlist(jordan_pts)
    return(pt_val[1])
  }
  
}