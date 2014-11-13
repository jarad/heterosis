#' A function to subset the data
#' 
#' This function will subset the data by RNAseq counts.
#' 
#' @param data data.frame. 
#' @param feature character string indicating the column that has the feature (gene)
#' @param genotype character string indicating the column that has the genotype identifier
#' @param mean_threshold minimum mean count across samples for a feature to be included
#' @param count_threshold minimum count in any sample for a feature to be included
#' 
#' @export
#' @return The subsetted data frame
#' @author Jarad Niemi
#' 
trim = function(data, feature="feature", genotype="genotype", mean_threshold=1, count_threshold=0) {
  if (!require(plyr)) {
    warning("Please install the plyr library.\nNo trimming performed.")
    return(data)
  }
  
  ddply(data, feature, function(x) {
    if (any(x$count   < count_threshold)) return(NULL)
    if (mean(x$count) < mean_threshold  ) return(NULL)
    
    return(x)
  })
}
