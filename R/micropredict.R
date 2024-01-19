#' micropredict()
#'
#' This function is the main function of MicroPredict R package
#' @import lme4
#' @import data.table
#' @param X abundance or lmm input form (dataframe / file path)
#' @param meta meta data 
#' @param preprocess preprocessed or not
#' @param normalized normalized or not
#' @param sep separator
#' @keywords MicroPredict main module
#' @return A imputed matrix of dimension n_taxa x n_samples
#' @export
#' @examples
#' micropredict()
#' 
#' 
micropredict <- function(X=smpdata, meta=metadata, preprocess=FALSE, normalized=TRUE, sep=","){
	
	options(warn=-1)
	cat("************************************************************\n")
	cat(" MicroPredict Version 1.0.0 \n")
	cat(" Copyright (C) 2023 Hanlab in Seoul National University (SNU)\n")
	cat(" Made by. Chloe Soohyun Jang\n")
	cat("************************************************************\n\n")
	cat("MicroPredict Prediction Start.\n")
	
	cat("Step 1. Preprocessing...\n")
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

	# Two modules
	## Update module
	cat("Step 2. Update module processing...\n")
	imputed_upd <- upd_module(lmm_input[["X"]])
	cat("Update module Completed.\n\n")
	
	## Imputation module
	cat("Step 3. Imputation module processing...\n")
	imputed_imp <- imp_module(smp_name,meta,imputed_imp[[2]],lmm_input[["pc"]])
	cat("Imputation module Completed.\n\n")
	
	
	
	if (preprocess==FALSE){
		exp_16sonly <- X[row.names(X) %in% imputed_upd[[2]],]
	}
	else {
		tmp <- dcast(X[,c('Species','Sample','exp_16s')], Species~Sample)
		tmp <- rownameset(tmp)
		exp_16sonly <- tmp[row.names(tmp) %in% imputed_upd[[2]],]
	}
	
	cat("Step 4. Merging imputed outputs from two modules...\n")

	int_species <- Reduce(intersect, list(row.names(imputed_imp), row.names(exp_16sonly), row.names(imputed_upd[[1]])))
	
	if (length(int_species) > 1){
		cat("  - Averaging several species duplicate imputed values from two different modules...\n")
		
		imputed_imp$species <- row.names(imputed_imp)
		exp_16sonly$species <- row.names(exp_16sonly)
		imputed_upd$species <- row.names(imputed_upd[[1]])

		row.names(imputed_imp) <- NULL
		row.names(exp_16sonly) <- NULL
		row.names(imputed_upd[[1]]) <- NULL

		imputed <- rbind(imputed_imp, exp_16sonly, imputed_upd[[1]])
		imputed_mean <- setDT(imputed)[, lapply(.SD, mean, na.rm=TRUE), by=species]
		row.names(imputed_mean) <- imputed_mean$species
		imputed_mean$species <- NULL
		imputed <- imputed_mean
		
	}
	
	else {
		cat(" * Update module species :", nrow(imputed_upd[[1]]), "\n")
		cat(" * Imputation module species :", nrow(imputed_imp), "\n")
		cat(" * 16S Only species :", nrow(exp_16sonly), "\n\n")
		
		imputed <- rbind(imputed_imp, exp_16sonly, imputed_upd[[1]])
		
		cat(" ⇢ Total species :", nrow(imputed), "\n")
	}
	
	cat("MicroPredict Finished.\n")
	cat("************************************************************\n")
	
	return(imputed)

}

