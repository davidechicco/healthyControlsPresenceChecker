#' Function that reads in the GEO code of a dataset, and returns true if there's at least a feature containing the healthy controls.
#'
#' @param datasetGeoCode the GEO code of a dataset.
#' @param verbose a boolean flag stating if helping messages should be printed or not
#' @export
#' @import geneExpressionFromGEO GEOquery xml2
#' @return a boolean value
#' @examples
#' healthyControlsCheckOutcome <- healthyControlsCheck("GSE3268", FALSE)
healthyControlsCheck <- function(datasetGeoCode, verbose = FALSE) 
{

            GSE_code <- datasetGeoCode
            healthyControlWordPresent <- NULL
            
            # check   URL
            checked_html_text <- "EMPTY_STRING"
            checked_html_text <- xml2::read_html("https://ftp.ncbi.nlm.nih.gov/geo/series/")
            
            checked_html_text_url <- "EMPTY_STRING"
            url_to_check <- paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", datasetGeoCode)
            GSE_code_for_url <- GSE_code
            GSE_code_for_url <- substr(GSE_code_for_url,1,nchar(GSE_code_for_url)-3)
            GSE_code_for_url <- paste0(GSE_code_for_url, "nnn")
            complete_url <- paste0("https://ftp.ncbi.nlm.nih.gov/geo/series/", GSE_code_for_url, "/", GSE_code)
           
           checked_html_text_url <- lapply(complete_url, geneExpressionFromGEO::readUrl)
           
           gset <- NULL
#             
            if(all(checked_html_text == "EMPTY_STRING")) {
         
                    message("The web url https://ftp.ncbi.nlm.nih.gov/geo/series/ is unavailable right now. Please try again later. The function will stop here\n")
                    return(NULL)
                    
            } else if(all(checked_html_text_url == "EMPTY_STRING" | is.null(checked_html_text_url[[1]]) )) {
         
                    message("The web url ", complete_url," is unavailable right now (Error 404 webpage not found). The GEO code might be wrong. The function will stop here\n")
                    return(NULL)        
                    
            } else {

	      gset <- GEOquery::getGEO(GSE_code,  GSEMatrix =TRUE, getGPL=FALSE)
	      
	      thisGEOplatform <- toString((gset)[[1]]@annotation)

	      if (length(gset) > 1) idx <- grep(thisGEOplatform, attr(gset, "names")) else idx <- 1
	      gset <- gset[[idx]]
	      
	      if(verbose == TRUE) message("=== === === === === ", GSE_code, " === === === === ===  \n")
	      
               if(verbose == TRUE) message("=== === === === === ", GSE_code, " === === === === ===  \n")
                
                healthyWordPresent <- grepl("healthy|Healthy", (gset@phenoData@data)) %>% any()
                if(healthyWordPresent == TRUE) {
                
                    if(verbose == TRUE) message(":: The keyword \"healthy\" was found in this dataset annotations (", GSE_code, ")\n")
                    healthy_indexes <- which(grepl("healthy", (gset@phenoData@data)))
		            if(verbose == TRUE)  message("on ", length(healthy_indexes), " feature(s)\n")
                    healthy_indexes <- which(grepl("healthy", (gset@phenoData@data)))
                    
                    countFeatures <- 1
                    for(i in healthy_indexes){
                        this_feature <- (gset@phenoData@data)[i] %>% colnames()  
                        if(verbose == TRUE)  message("\n(", countFeatures, ") \"", this_feature, "\" feature\n")
                        if(verbose == TRUE) (gset@phenoData@data)[i] %>% table() %>% print()
                        
                        thisFeatureGroups <- (gset@phenoData@data)[i] %>% table()
                        thisFeatureGroupsNames <- thisFeatureGroups %>% names()
                        numGroupsInThisFeature <- thisFeatureGroups  %>% nrow()

                        for(k in 1:length(thisFeatureGroups)) {
                                if(verbose == TRUE)  message(thisFeatureGroupsNames[k], ": ")
                                thisFeatureGroupPerc <- thisFeatureGroups[[k]] * 100 / (gset@phenoData@data) %>% nrow()
                                if(verbose == TRUE)  message("\t", geneExpressionFromGEO::dec_two(thisFeatureGroupPerc), "%\n")
                        }
                        
                        countFeatures <- countFeatures + 1
                    }
                } else { 
                    if(verbose == TRUE) message(":: The keyword \"healthy\" was NOT found among the annotations of this dataset (", GSE_code, ")\n") 
                }     
                
                
           healthyControlWordPresent <- grepl("control|Controlcontrols|Controls", (gset@phenoData@data)) %>% any()
	      if(healthyControlWordPresent == TRUE) {
	      
		       if(verbose == TRUE) message(":: The keyword \"control\" was found in this dataset annotations (", GSE_code, ") ")
		       healthy_control_indexes <- which(grepl("control", (gset@phenoData@data)))
		       if(verbose == TRUE)  message("on ", length(healthy_control_indexes), " feature(s)\n")
		       
		       countFeatures <- 1
		       
		       for(i in healthy_control_indexes){
                    this_feature <- (gset@phenoData@data)[i] %>% colnames()  
                    if(verbose == TRUE)  message("\n(", countFeatures, ") \"", this_feature, "\" feature\n")
                    if(verbose == TRUE) (gset@phenoData@data)[i] %>% table() %>% print()
                    
                    thisFeatureGroups <- (gset@phenoData@data)[i] %>% table()
                    thisFeatureGroupsNames <- thisFeatureGroups %>% names()
                    numGroupsInThisFeature <- thisFeatureGroups  %>% nrow()

                    for(k in 1:length(thisFeatureGroups)) {
                            message(thisFeatureGroupsNames[k], ": ")
                            thisFeatureGroupPerc <- thisFeatureGroups[[k]] * 100 / (gset@phenoData@data) %>% nrow()
                            if(verbose == TRUE)  message("\t", geneExpressionFromGEO::dec_two(thisFeatureGroupPerc), "%\n")
                    }
                    
                    countFeatures <- countFeatures + 1
		          }
                } else { 
                    if(verbose == TRUE) message(":: The keyword \"control\" was NOT found among the annotations of this dataset (", GSE_code, ")\n") 
                }            
            }
                        
            if(verbose == TRUE)  message("=== === === === === === === === === === === ===  \n")
            
            outcome <- (healthyControlWordPresent | healthyWordPresent)
            if(verbose == TRUE)  message("\nhealthyControlsCheck() call output: were healthy controls found in the ", GSE_code, " dataset? ", outcome, "\n")
            return(outcome)
}   

  
