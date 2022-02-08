check_data<- function(...){
  # make this into a RMD
  nm<-as.list(substitute(list(...)))[-1L]
  dots<- list(...)
  names(dots)<-nm

  # Check for missing values
  map(dots, function(i){
    colSums(is.na(i))
  })

# Check that all units are the same
  dots$adsl[, unique(DIABDURU)]
  dots$adlb[, unique(AVALU)]

}

