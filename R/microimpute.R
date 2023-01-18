#' microimpute()
#'
#' This function is the main function of MicroImpute R package
#' @import lme4
#' @import reshape2
#' @import data.table
#' @param X abundance 
#' @param meta meta data 
#' @param preprocess preprocessed or not
#' @param normalized normalized or not
#' @param sep separator
#' @keywords MicroImpute main module
#' @return A imputed matrix of dimension n_taxa x n_samples
#' @export
#' @examples
#' microimpute()
#' 
#' 
microimpute <- function(X=smpdata, meta=meta, preprocess=FALSE, normalized=TRUE, sep=","){
	
	options(warn=-1)
	
	cat("Preprocessing...\n")
	if (preprocess==FALSE) {
		if (normalized==TRUE) {
			lmm_input <- preprocess(X, meta, normalized=TRUE, sep=",")
		}

		else {
			lmm_input <- preprocess(X, meta, normalized=FALSE, sep=",")
		}
	}

	else {
		lmm_input <- list()

		if (class(X) == "character") {
			X <- as.data.frame(fread(X, sep=","))
		}
		tmp <- X
		tmp <- dcast(tmp[,c('Species','Sample','exp_16s')], Species~Sample)
		tmp <- rownameset(tmp)

		# 10PCs
		pc_s <- prcomp(rbind(tmp))$rotation[,1:10]
		lmm_input[["pc"]] <- pc_s

		if (normalized==TRUE) {
			lmm_input[["X"]] <- X
		}
		else {
			# normalization
			tmp <- log10((tmp/colSums(tmp))+1.01)

			tmp <- melt(as.matrix(tmp))
			colnames(tmp) <- c('Species','Sample','exp_16s')
			
			if(ncol(tmp)>3){
				merged <- merge(tmp, subset(X, select = -c(exp_16s)), by=c("Species","Sample"))
			}
			lmm_input[["X"]] <- merged
		    }
	}

	smp_name <- levels(lmm_input[["X"]]$Sample)
	cat("Preprocessed Completed.\n\n")

	# two modules
	cat("Update module processing...\n")
	imputed_upd <- upd_module(lmm_input[["X"]])
	cat("Update module Completed.\n")
	
	cat("Imputation module processing...\n")
	imputed_imp <- imp_module(smp_name,meta,imputed_imp[[2]],lmm_input[["pc"]])
	cat("Imputation module Completed.\n")
	
	
	
	if (preprocess==FALSE){
		exp_16sonly <- X[row.names(X) %in% setdiff(imputed_imp[[2]], imputed_upd[[2]]),]
	}
	else {
		tmp <- dcast(X[,c('Species','Sample','exp_16s')], Species~Sample)
		exp_16sonly <- tmp[row.names(tmp) %in% setdiff(imputed_imp[[2]], imputed_upd[[2]]),]
	}

	cat("Merging imputed outputs from two modules...\n")
	tryCatch({
		imputed <- rbind(imputed_imp[[1]], exp_16sonly, imputed_upd[[1]])
	},
	error = function(e) {
		cat("  - Averaging several species duplicate imputed values from two different modules...\n")
		int_species <- intersection(row.names(imputed_imp[[1]]), row.names(exp_16sonly), row.names(imputed_upd[[1]]))
		if (int_species>1){

			imputed_imp$species <- row.names(imputed_imp[[1]])
			exp_16sonly$species <- row.names(exp_16sonly)
			imputed_upd$species <- row.names(imputed_upd[[1]])

			row.names(imputed_imp[[1]]) <- NULL
			row.names(exp_16sonly) <- NULL
			row.names(imputed_upd[[1]]) <- NULL

			imputed <- rbind(imputed_imp[[1]], exp_16sonly, imputed_upd[[1]])
			imputed_mean <- setDT(imputed)[, lapply(.SD, mean, na.rm=TRUE), by=species]
			row.names(imputed_mean) <- imputed_mean$species
			imputed_mean$species <- NULL

	}

	})
	
	cat("MicroImpute Finished.")
	return(imputed)

}

