#' preprocess() function
#'
#' This function preprocess the input data and transform the data for imputation
#' @param abd_loc : abundance data location for import
#' @param meta : metadata location for import
#' @param sep : separator for data
#' @keywords preprocess
#' @export
#' @examples
#' preprocess()
#' 
#' 
rownameset <- function(df){
	df <- as.data.frame(df)
	row.names(df) <- df$taxa
	df$Species <- NULL

	return(df)
}

preprocess <- function(abd_loc, meta=meta, normalized=TRUE, sep=","){

	# row: microbiome x col: sample name (m x n)
	abd = as.data.frame(fread(abd_loc, sep=sep))
	meta = as.data.frame(fread(meta_loc, sep=sep)) # Sample cov1 cov2

	row.names(abd) <- abd[,1]
	abd <- abd[,-1]

	if (normalized==FALSE){
		abd <- log10((abd/colSums(abd))+1.01)
	}

	# 10PCs
	pc_s <- prcomp(rbind(abd))$rotation[,1:10]

	melted <- melt(as.matrix(abd))
	colnames(melted) <- c("Species","Sample","exp_16s")
	colnames(meta) <- c("Sample","cov1_cm","cov2_tp")

	# add cov columns
	meta$cov1_cm <- ifelse(meta$cov1_cm == 1,"C","M")
	if (is.element("first", meta$cov2_tp)){
	meta$cov2_tp <- ifelse(meta$cov2_tp == "first",1,2)}

	meta$cov1_cm <- as.factor(meta$cov1_cm)
	meta$cov2_tp <- as.integer(meta$cov2_tp)

	X = merge(melted, meta, by = c("Sample"))

	lmm_input <- list()
	lmm_input[["X"]] <- X
	lmm_input[["pc"]] <- pc_s

	return(lmm_input)

}