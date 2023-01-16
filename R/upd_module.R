#' upd_module() function
#'
#' This function is update module for species detected from both platforms (16S and WGS)
#' @param X after preprocess step 
#' @keywords imputation module
#' @export
#' @examples
#' imp_module())
#' 
#'
upd_module <- function(X=lmm_input){

	# load model
	lmm_upd <- readRDS(paste(.libPaths(),"/MicroImpute/model/upd_module.rds",sep=""))

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
