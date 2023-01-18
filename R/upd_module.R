#' upd_module() function
#'
#' This function is update module for species detected from both platforms (16S and WGS)
#' @import lme4
#' @import data.table
#' @param X after preprocess step 
#' @keywords update module
#' @export
#' @examples
#' upd_module())
#' 
#'
upd_module <- function(X=lmm_input){

	# load update module trained model
	githubURL <- "https://github.com/hanlab-SNU/MicroImpute/raw/main/model/upd_module.rds"
	download.file(githubURL,"upd_module.rds", method="wget")
	lmm_upd <- readRDS("upd_module.rds")

	species_notimp <- X[!X$Species %in% lmm_upd@frame$Species,]$Species

	# check species for imputation module
	X <- X[X$Species %in% lmm_upd@frame$Species,]
	X$exp_imp <- predict(lmm_upd, X)

	imputed <- list()
	imputed[[1]] <- dcast(X[,c('Species','Sample','exp_imp')], Species~Sample)
	imputed[[1]] <- rownameset(imputed)

	imputed[[2]] <- species_notimp

	return(imputed)

}
