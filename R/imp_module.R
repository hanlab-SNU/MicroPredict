#' imp_module() function
#'
#' This function is imputation module for species detected only from WGS
#' @import lme4
#' @import data.table
#' @param X after preprocess step 
#' @keywords imputation module
#' @export
#' @examples
#' imp_module())
#' 
#' 
imp_module <- function(smp_name=smp_name, meta=meta, species_upd = species_upd, pc=pc){

	# load imputation module trained model
	githubURL <- "https://github.com/hanlab-SNU/MicroImpute/raw/main/model/imp_module.rds"
	download.file(githubURL,"imp_module.rds", method="wget")
	lmm_imp <- readRDS("imp_module.rds")

	# making lmm_input for imputation module
	X = melt(matrix(0, nrow=length(lmm_imp@frame$Species), ncol=length(smp_name), dimnames=list(lmm_imp@frame$Species,smp_name)))[,1:2]
	colnames(X) <- c("Species","Sample")
	X <- merge(X, metadata, by=c("Sample"))

	# add PC columns depending on sample ID - species
	X[,paste("PC",rep(1:10),sep="")] <- NA

	
	for (i in smp_name){
		X[X$Sample == i,][5:14] <- pc[i,]
	}
	
	# imputation
	X$exp_imp <- predict(lmm_imp, X)

	imputed <- dcast(X[,c('Species','Sample','exp_imp')], Species~Sample)
	imputed <- rownameset(imputed)

	return(imputed)

}

