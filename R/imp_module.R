#' imp_module() function
#'
#' This function is imputation module for species detected only from WGS
#' @param X after preprocess step 
#' @keywords imputation module
#' @export
#' @examples
#' imp_module())
#' 
#' 
imp_module <- function(smp_name=smp_name, meta=meta, species_upd = species_upd, pc=pc){

	# model
	lmm_imp <- readRDS(paste(.libPaths(),"/MicroImpute/model/imp_module.rds",sep=""))

	# making lmm_input for imputation module
	X = melt(matrix(0, nrow=length(lmm_upd@frame$Species), ncol=length(smp_name), dimnames=list(lmm_upd@frame$Species,smp_name)))[,1:2]
	colnames(X) <- c("Speceis","Sample")
	X <- merge(X, metadata, by=c("Sample"))

	# add PC columns depending on sample ID - species
	X[,paste("PC",rep(1:10),sep="")] <- NA
	for (i in 1:nrow(X)){
		X[i,6:15] <- pc[as.character(X$Sample[[i]]),]
	}

	# imputation
	X$exp_imp <- predict(lmm_imp, X)

	imputed <- dcast(X[,c('Species','Sample','exp_imp')], Species~Sample)

	return(imputed)

}

