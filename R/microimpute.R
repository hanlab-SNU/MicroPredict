#' microimpute()
#'
#' This function is the main function of MicroImpute R package
#' @param X 
#' @param meta
#' @param preprocess 
#' @param sep
#' @keywords imputation module
#' @return A imputed matrix of dimension n_taxa x n_samples
#' @export
#' @examples
#' microimpute()
#' 
#' 
microimpute <- function(X=test_data, meta=meta, preprocess=FALSE, normalized=TRUE, sep=","){

	if (preprocess==FALSE){
		if (normalized==TRUE){
			lmm_input <- preprocess(abd_loc=X, meta=meta, normalized=TRUE, sep=",")
		}

		else{
			lmm_input <- preprocess(abd_loc=X, meta=meta, normalized=FALSE, sep=",")
		}
	}

	else{
		lmm_input <- list()

		X <- as.data.frame(fread(X, sep=","))
		tmp <- X
		tmp <- dcast(tmp[,c('Species','Sample','exp_16s')], Species~Sample)
		tmp <- rownameset(tmp)

		# 10PCs
		pc_s <- prcomp(rbind(tmp))$rotation[,1:10]
		lmm_input[["pc"]] <- pc_s

		if (normalized==TRUE){
			lmm_input[["X"]] <- X
		}
		else{
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

	smp_name <- unique(lmm_input$Sample)

	# two modules
	imputed_upd <- upd_module(lmm_input[["X"]])
	imputed_imp <- imp_module(smp_name,meta,imputed_imp[[2]],lmm_inut[["pc"]])
	
	if (preprocess==FALSE){
		exp_16sonly <- X[row.names(X) %in% setdiff(imputed_imp[[2]], imputed_upd[[2]]),]
	}
	else{
		tmp <- dcast(X[,c('Species','Sample','exp_16s')], Species~Sample)
		exp_16sonly <- tmp[row.names(tmp) %in% setdiff(imputed_imp[[2]], imputed_upd[[2]]),]
	}

	result = tryCatch({
		imputed <- rbind(imputed_imp[[1]], exp_16sonly, imputed_upd[[1]])
	},
	}, error = function(e) {
		print("Averaging several species duplicate imputed values from two different modules...")
		int_species <- intersection(row.names(imputed_imp[[1]]), row.names(exp_16sonly), row.names(imputed_upd[[1]]))
		if (int_species>1){


	}

	}, finally = {NULL
	}
	)
	

	return(imputed)

}
