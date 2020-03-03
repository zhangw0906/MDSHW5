show_correlation<- function(df , year = 1986) {
  
  # read the data
  bulk_data = df
  # select the year.
  filter(bulk_data , Year == year)  %>%      # filter by the  indicated year
    keep(is.numeric ) -> numeric_cols         # only keep numeric columns
  
  within(numeric_cols, rm(Year)) -> for_cor # this is done to remove NA from cor  (0 variance )
  c = cor(for_cor , use = "complete.obs" )
  corrplot(c, method="circle")
  
}