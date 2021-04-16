# Franken Package
# Franken contains basic and core functions required for running Franken. Some of these functions are borrowed and sometimes modified versions of functions from the wonderful package flowSOM. I've attempted to point out where it happens

#' Franken Package
#'
#' Contains functions required for Franken clustering procedure
#'
#' @doctype Package
#'
#' @author Laura De Vargas Roditi \email{laura.devargasroditi@uzh.ch}
#'
#' @name Franken
NULL

#' Row-wise max of a data frame
#'
#' Returns the maximum value of a data frame's row
#'
#' @author Laura De Vargas Roditi \email{laura.devargasroditi@uzh.ch}
#'
#' @param df data frame
#'
#' @return maximum value of a data frame's row
#'
#' @export
rowMax <- function(df){

    df = apply(df, 1, function(x) max(x, na.rm = TRUE))
}

#' Rank of a column of a data frame
#'
#' Returns the maximum value of a data frame's row
#'
#' @param df data frame
#'
#' @return maximum value of a data frame's row
#'
#' @export
colRank <- function (df){

    df = as.data.frame(apply(df, 2, function(x) rank(x)))
    # df2 = apply(df, 2, function(x) rank(x))
}


#' Column-wise max of a data frame
#'
#' Returns the maximum value of a data frame's row
#'
#' @param df data frame
#'
#' @return maximum value of a data frame's row
#'
#' @export
colMax <- function(df){

    df = as.data.frame(apply(df, 2, function(x) max(x, na.rm = TRUE)))
}

#' Read fcs-files, flowframes, data tables or data frames
#'
#' modified readinput function from flowSOM
#' for cases where data is not in fcs format but a data.table/frame
#'
#' Take some input and return flowSOM object containing a matrix with
#' the preprocessed data (compensated, transformed, scaled)
#'
#' @param input         a flowFrame, a flowSet or an array of paths to files
#'                      or directories or a data table or data frame
#' @param pattern       if input is an array of file- or directorynames,
#'                      select only files containing pattern
#' @param compensate    logical, does the data need to be compensated
#' @param spillover     spillover matrix to compensate with
#'                      If \code{NULL} and compensate=\code{TRUE}, we will
#'                      look for \code{$SPILL} description in fcs file.
#' @param transform     logical, does the data need to be transformed
#' @param toTransform   column names or indices that need to be transformed.
#'                      If \code{NULL} and transform=\code{TRUE}, column names
#'                      of \code{$SPILL} description in fcs file will be used.
#' @param transformFunction Defaults to logicleTransform()
#' @param scale         logical, does the data needs to be rescaled
#' @param scaled.center see \code{\link{scale}}
#' @param scaled.scale  see \code{\link{scale}}
#' @param silent        if \code{TRUE}, no progress updates will be printed
#'
#' @return flowSOM object containing the data, which can be used as input
#' for the BuildSOM function
#'
#' @seealso \code{\link{scale}},\code{\link{BuildSOM}}
#'
#' @examples
#' # Read from file
#' fileName <- system.file("extdata", "68983.fcs", package="flowSOM")
#' flowSOM.res <- ReadInputfrkn(fileName, compensate=TRUE,transform=TRUE,
#'                          scale=TRUE)
#'
#' # Or read from flowFrame object
#' ff <- flowCore::read.FCS(fileName)
#' ff <- flowCore::compensate(ff,ff@@description$SPILL)
#' ff <- flowCore::transform(ff,
#'                  flowCore::transformList(colnames(ff@@description$SPILL),
#'                                  flowCore::logicleTransform()))
#' flowSOM.res <- ReadInput(ff,scale=TRUE)
#'
#' @export
ReadInputfrkn <-function (input, pattern = ".fcs", compensate = FALSE, spillover = NULL,
                         transform = FALSE, toTransform = NULL, transformFunction = flowCore::logicleTransform(),
                         scale = FALSE, scaled.center = TRUE, scaled.scale = TRUE,
                         silent = FALSE)
{
    fsom <- list(pattern = pattern, compensate = compensate,
        spillover = spillover, transform = transform, toTransform = toTransform,
        transformFunction = transformFunction, scale = scale)
    class(fsom) <- "flowSOM"

    if (all(class(input) == "flowFrame") | all(class(input) == c("data.table", "data.frame"))) {
        fsom <- AddFlowFramefrkn(fsom, input)
    } else if (class(input) == "data.frame"){
            input <- as.data.table(input)
            fsom  <- AddFlowFramefrkn(fsom, input)
    } else if (class(input) == "flowSet") {
        for (i in seq_along(input)) {
            fsom <- AddFlowFramefrkn(fsom, input[[i]])
        }
    }
    else if (class(input) == "character") {
        toAdd <- NULL
        toRemove <- NULL
        for (i in seq_along(input)) {
            if (file.info(input[i])$isdir) {
                toAdd <- c(toAdd, list.files(input[i], pattern = pattern,
                  full.names = TRUE))
                toRemove <- c(toRemove, i)
            }
        }
        if (!is.null(toRemove)) {
            input <- c(input[-toRemove], toAdd)
        }
        input <- grep(pattern, input, value = TRUE)
        if (length(input) > 0) {
            for (i in seq_along(input)) {
                if (file.exists(input[i])) {
                  if (!silent)
                    message("Reading file ", input[i], "\n")
                  if (tools::file_ext(input[i]) == "csv") {
                    flowFrame <- flowFrame(utils::read.table(input[i]))
                  }
                  else {
                    flowFrame <- suppressWarnings(flowCore::read.FCS(input[i]))
                  }
                  fsom <- AddFlowFramefrkn(fsom, flowFrame)
                }
            }
        }
        else {
            stop("No files containing the pattern are found.")
        }
    }else{
        stop(paste("Inputs of type", class(input), "are not supported. \n                    Please supply either a FlowFrame, a FlowSet or an array\n                    of valid paths to files or directories."))
    }
    if (scale) {
        if (!silent)
            message("Scaling the data\n")
        fsom$data <- scale(fsom$data, scaled.center, scaled.scale)
        fsom$scaled.center <- attr(fsom$data, "scaled:center")
        attr(fsom$data, "scaled:center") <- NULL
        fsom$scaled.scale <- attr(fsom$data, "scaled:scale")
        attr(fsom$data, "scaled:scale") <- NULL
    }
    fsom
}

#' Prepare data for Franken run
#'
#' Arcsinh's data and changes .fcs file names to more useful ones as per the user's choosing
#'
#' @param dataDir           Directory where data .fcs files can be found
#' @param file_trim_regex   Regular expression to identify in the data files' names to be replaced
#' @param file_trim_replace Expression to replace in data files' names
#'
#' @return Normalized data (arcsinh factor 5) and modified file names according to user's choice
#'
#' @export
Franken_data_prep <- function(dataDir, file_trim_regex = NULL, file_trim_replace = NULL){
# Have all (and only) fcs files that you want to scale in the same folder DataDir

    #################
    ### LOAD DATA ###
    #################

    currdir                    = getwd()
    DataDir                    = dataDir
    # if no path provided to SaveDir, results will be saved in a folder in DataDir (see below)
    # SaveDir                          = "";

    # List all .fcs files in this folder
    files_full_path            = list.files(path = DataDir, pattern = "\\.fcs$", all.files = FALSE, full.names = TRUE) # includes full path
    files_names                = list.files(path = DataDir, pattern = "\\.fcs$", all.files = FALSE, full.names = FALSE)

    # Go to data directory
    setwd(DataDir)

    ## Trim file names differently depending on specific datasets
    ## ^ beginning of expression
    ## [_]* matches underscore at least 0 times
    ## [^...] matches any charater EXCEPT the one inside the square brackets
    ## (?=) zero-width positive look ahead assertion
    ## $ matches the end of a string
    if(!is.null(file_trim_regex)){

            if(is.null(file_trim_replace)){

                file_trim_replace = ""
                warning('If providing a regular expression to search, you have to provide a replacing element or the default will be no replacement at all')
            }
    # Examples
    # Trim off everything before first underscore (first from left to right).
    # pop_names   = gsub(".*_", "", file_names)

    # Trim off everything after first underscore ()/
    # pop_names   = gsub("(.*?)(_.*)", "\\1", pop_names)

    ## Replace everything after FIRST underscore with ".fcs"
    # pop_names   = gsub("_.*", ".fcs", files_names)

    # Replace everything after SECOND underscore with .fcs
    # pop_names = gsub("^([^_]*_[^_]*)_.*$", "\\1.fcs", files_names)
    pop_names = gsub(file_trim_regex, file_trim_replace, files_names)

    }else{

        pop_names       = files_names
    }

    asinh_scale     = 5

    # If no directory provided to save data then default to creating a directory to store arcsinh'ed data in the original DataDir
    if (exists('SaveDir')){
      asinh_dir           = SaveDir
      if (!(dir.exists(asinh_dir))){
      }
    }else{
      asinh_dir           = file.path(DataDir,'asinh')
    }
    dir.create(asinh_dir)

    # arcsinh transform and export file by file
    for (ii in 1:length(files_full_path)) {

      old_flow_frame = flowCore::read.FCS(files_full_path[ii], transformation = FALSE, truncate_max_range = FALSE)

      data_i         = flowCore::exprs(old_flow_frame)

      if (exists('cols_to_scale')){
        data_i[, cols_to_scale] <- asinh(data_i[, cols_to_scale] / asinh_scale)
      } else{
        data_i <- asinh(data_i / asinh_scale)
      }

      new_flow_frame        = old_flow_frame
      new_flow_frame@exprs  = data_i

      flowCore::write.FCS(new_flow_frame, filename = file.path(asinh_dir, pop_names[ii]))

    } # ii
}

#' Add a flowFrame to the data variable of the flowSOM object
#'
#' Modified version of AddFlowFrame from flowSOM to accept data.frame or data.table.
#' Beware that feeding data.frame or data.table to Franken means you might be missing some meta-data usually found in the originall .fcs files.
#'
#' @param fsom      flowSOM object, as constructed by the ReadInputfrkn function
#' @param flowFrame flowFrame to add to the flowSOM object
#'
#' @return flowSOM object with data added
#' @export
AddFlowFramefrkn <-function (fsom, flowFrame)
{
    if (all(class(flowFrame) == c("data.table", "data.frame"))){

        warning('Please ensure your data is in mxn format where m are observations and n are markers')
        warning('Only numerical data and their column labels being extracted')
        warning('No scaling applied to this data type. If you wish to analyze scaled data please provide an already scaled data.table')

        # Select only numerical columns to avoid extra labels
        nums                <- unlist(lapply(flowFrame, is.numeric))
        numflowFrame        <- as.matrix(flowFrame[ ,..nums])
        fsom$data           <- numflowFrame
        # Select only column names of numerical columns
        fsom$prettyColnames <- colnames(flowFrame)[nums]


    } else if (all(class(flowFrame) != c("data.table", "data.frame"))){


        if (fsom$compensate) {
            if (is.null(fsom$spillover)) {
                if (!is.null(flowFrame@description$SPILL)) {
                    fsom$spillover <- flowFrame@description$SPILL
                }
                else if (!is.null(flowFrame@description$`$SPILLOVER`)) {
                    if (class(flowFrame@description$`$SPILLOVER`) ==
                      "matrix") {
                      fsom$spillover = flowFrame@description$`$SPILLOVER`
                      flowFrame@description$SPILL = fsom$spillover
                    }
                    else {
                      spilloverStr <- strsplit(flowFrame@description$`$SPILLOVER`,
                        ",")[[1]]
                      n <- as.numeric(spilloverStr[1])
                      fsom$spillover <- t(matrix(as.numeric(spilloverStr[(n +
                        2):length(spilloverStr)]), ncol = n))
                      colnames(fsom$spillover) <- spilloverStr[2:(n +
                        1)]
                      flowFrame@description$SPILL <- fsom$spillover
                    }
                }
                else {
                    stop("No compensation matrix found")
                }
            }
            flowFrame <- flowCore::compensate(flowFrame, fsom$spillover)
        }
        if (fsom$transform) {
            if (is.null(fsom$toTransform)) {
                fsom$toTransform <- colnames(flowFrame@description$SPILL)
            }
            else {
                fsom$toTransform <- colnames(flowCore::exprs(flowFrame)[,
                    fsom$toTransform])
            }
            flowFrame <- flowCore::transform(flowFrame, flowCore::transformList(fsom$toTransform,
                fsom$transformFunction))
        }

        n <- flowFrame@parameters@data[, "name"]
        d <- flowFrame@parameters@data[, "desc"]

        d[is.na(d)] <- n[is.na(d)]

        fsom$prettyColnames <- d

        # if (any(grepl("#", d))) {
            # fsom$prettyColnames <- gsub("#(.*)$", " (\\1)", d)
        if (any(grepl("_", d))) {
             fsom$prettyColnames <- gsub(".*_", "", d)
        }
        # else {
            # fsom$prettyColnames <- paste(d, " <", n, ">", sep = "")
        # }

        names(fsom$prettyColnames) <- fsom$prettyColnames

        # names(fsom$prettyColnames) <- colnames(flowCore:::exprs(flowFrame))
        # names(fsom$prettyColnames) <- flowCore::::parameters(flowFrame)$desc


        f <- flowCore::exprs(flowFrame)
        attr(f, "ranges") <- NULL
        name <- flowFrame@description$FIL
        if (is.null(name))
            name <- flowFrame@description$`$FIL`
        if (is.null(name))
            name <- length(fsom$metaData) + 1
        if (is.null(fsom$data)) {
            fsom$data <- f
            fsom$metaData <- list()
            fsom$metaData[[name]] <- c(1, nrow(fsom$data))
        }
        else {
            fsom$data <- rbind(fsom$data, f)
            fsom$metaData[[name]] <- c(nrow(fsom$data) - nrow(f) +
                1, nrow(fsom$data))
        }
    }
    fsom
}


#' Updates flowSOM object after data subsetting
#'
#' @param readin    flowSOM object, as constructed by the ReadInputfrkn function
#' @param new_ids   Row ids of cells to include in subsetted data
#'
#' @return Subsetted and updated flowSOM object
#' @export
update_ReadInput_file <- function(readin, new_ids){

    readin_sub                  = readin
    readin_sub$data             = readin_files$data[new_ids,]
    expanded_files_list         = lapply(1:length(readin$metaData), function(y) rep(names(readin$metaData[y]), readin$metaData[[y]][2]-readin$metaData[[y]][1]+1) )
    files_array                 = unlist(expanded_files_list)
    intersect_files             = files_array[new_ids]
    test                        = data.table(intersect_files)
    counts_dt                   = test[, list(count = .N), by = intersect_files]

    # re-setup the list containing the beginning and end (in the list of cells) of each file (metadata)
    new_file_order.df           = cbind( c(1, 1+cumsum(counts_dt$count)[-length(counts_dt$count)]), cumsum(counts_dt$count))
    rownames(new_file_order.df) = counts_dt$intersect_files
    # Turn it into a list type
    new_file_order.list         = split(new_file_order.df, seq(nrow(new_file_order.df)))
    new_file_order.list         = setNames(split(new_file_order.df, seq(nrow(new_file_order.df))), rownames(new_file_order.df))
    readin_sub$metaData         = new_file_order.list

    readin_sub
}

#' Builds k-nearest neighbor graph
#'
#' @param fsom          flowSOM object, as constructed by the ReadInputfrkn function
#' @param kn            Number of nearest neighbors used to build the mutual KNN graph
#' @param dist_method   Distance or Similarity function used to build KNN graph. Extended Jaccard Similarity is the recommended default
#' @param silent        If \code{TRUE}, no progress updates will be printed
#' @param tSNE          If \code{TRUE}, an alternative tSNE layout is computed as well
#'
#' @return Subsetted and updated flowSOM object
#' @export
BuildKNNGfrkn <- function (fsom, kn, dist_method, silent = FALSE, tSNE = FALSE)
{
    fsom$MST <- list()
    if (!silent)
        message("Building KNN\\n")

    fsom$MST$graph <- as.undirected(cccd::nng(fsom$map$codes, k=kn, method=dist_method, mutual = TRUE))

    V(fsom$MST$graph)$name <- as.character(1:nrow(fsom$map$codes))

    fsom$MST$l <- igraph::layout.kamada.kawai(fsom$MST$graph)

    if (tSNE) {
        fsom$MST$l2 <- tsne(fsom$map$codes)
    }

    UpdateNodeSize(fsom)
}

#' Performs walktrap clustering of knn graph on SOM nodes
#'
#' @param fsom          flowSOM object, as constructed by the ReadInputfrkn function. Make sure to run BuildKNNGfrkn first
#' @param saveDir       Path to directory in which to save results
#' @param fseed         Walktrap finds communities in a graph via random walks which means results will differ slightly with each run. With fseed you can set your own seed for reproducilibity
#'
#' @return A file called metaclusterIds.txt containing clustering ids for each cell and a  object including clustering ids for each SOM node
#' @export
walk_metaclustering <- function(fsom, saveDir, fseed){

    set.seed(fseed)

    meta_results         = igraph::cluster_walktrap(fsom$MST$graph, steps = 3,
      merges = TRUE, modularity = TRUE, membership = TRUE)

    meta_clusterings     = meta_results$membership

    # ## Find consensus clustering
    meta_method          = "walktrap"
    consensus_ids        = meta_clusterings

    # estimate meta cluster for every cell and save output
    som_clusters         = fsom$map$mapping[,1]
    som_meta_clusters    = consensus_ids[som_clusters]

    write.table(som_meta_clusters, file = file.path(saveDir,"metaclusterIds.txt"), quote = FALSE, sep = "\n", row.names = FALSE, col.names = FALSE)
    list(consensus_ids = consensus_ids, meta_method = meta_method)
}

#' Clusters high dimensional data
#'
#' This function takes in single-cell data from fcs files, flowframes, flowsets or data.table/frames
#' @param readin_files      flowSOM object containing data matrix, generated by running ReadInputfrkn first
#' @param resultsDir        Directory to store outputs
#' @param marker_ids        An array of ids of which data columns to use for clustering
#' @param fseed             Seed to be used for SOM and KNN building step. Setting your own seed is highly encouraged
#'                          for reproducibility
#' @param xd                Width of the SOM grid
#' @param yd                Height of the SOM grid
#' @param kn                Number of nearest neighbors used to build the mutual KNN graph.
#' @param dist_method       Distance or Similarity function used to build KNN graph. Extended Jaccard Similarity
#'                          is the recommended default.
#'
#' @return A \code{list} containing three items: 1) a flowSOM object
#'         2) metaclustering results on SOM nodes and 3) a matrix of cellsXmarkers outputs
#'
#' @return A \code{list} with two items: the first is the flowSOM object
#'         containing all information (see the vignette for more detailed
#'         information about this object), the second is the metaclustering of
#'         the nodes of the grid. This is a wrapper function for
#'         \code{\link{ReadInput}}, \code{\link{BuildSOM}},
#'         \code{\link{BuildKNNGfrkn}} and \code{\link{walk_metaclustering}}.
#'         Executing them separately may provide more options.
#'
#' @import     cccd
#' @import     proxy
#' @import     data.table
#' @import     scales
#' @import     colorRamps
#' @import     dplyr
#' @import     gplots
#' @importFrom BiocGenerics colnames
#' @importFrom flowCore read.FCS compensate transform logicleTransform exprs
#'             transformList write.FCS 'exprs<-' keyword
#' @importFrom flowWorkspace openWorkspace getNodes
#'             keyword
#' @importFrom CytoML parseWorkspace
#' @importFrom igraph graph.adjacency minimum.spanning.tree layout.kamada.kawai
#'             plot.igraph add.vertex.shape get.edges shortest.paths E V 'V<-'
#'             igraph.shape.noclip cluster_walktrap
#' @importFrom methods is
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats prcomp
#' @importFrom Rtsne Rtsne
#' @importFrom utils capture.output
#' @importFrom XML xmlToList xmlParse
#' @export

Franken <- function(readin_files, resultsDir, marker_ids, fseed=NULL, xd = 20, yd = 20, kn = 6, dist_method = 'ejaccard'){

    markerOutputs           = data.table(readin_files$data[,marker_ids])
    colnames(markerOutputs) = as.character(readin_files$prettyColnames[marker_ids])

    # # Save matrix of cells versus marker values from this run
    # write.table(markerOutputs, file.path(resultsDir,'markerOutputs.txt'),
    #                                     col.names=TRUE, row.names=FALSE, sep=",")

    if (!is.null(fseed)){
    set.seed(fseed) # Encouraged for reproducibility
    }
    read_aff_SOM            = BuildSOMfrkn(readin_files, colsToUse=marker_ids, xdim = xd , ydim = yd )

    if (!is.null(fseed)){
    set.seed(fseed) # Encouraged for reproducibility
    }
    read_aff_SOM            = BuildKNNGfrkn(read_aff_SOM, kn = kn, dist_method = dist_method)

    # save(read_aff_SOM, file = file.path(resultsDir,'som.RData'))

    consensus_output        = walk_metaclustering(read_aff_SOM, resultsDir, fseed)

    SOMList                 = list("read_aff_SOM" = read_aff_SOM, "metaclus" = consensus_output, "markerOutputs" = markerOutputs)

    return(SOMList)
}

#' Build self-organizing map (borrowed from FlowSOM)
#'
#' @param  fsom
#' @param  colsToUse
#'
#' @export
BuildSOMfrkn <- function(fsom, colsToUse = NULL, silent = FALSE, ...)
{
  if (!"data" %in% names(fsom)) {
    stop("Please run the ReadInput function first!")
  }
  if (!silent)
    message("Building SOM\\n")
  if (is.null(colsToUse)) {
    colsToUse <- seq_len(ncol(fsom$data))
  }
  fsom$map <- FlowSOM::SOM(fsom$data[, colsToUse], silent = silent,
                  ...)
  fsom$map$colsUsed <- colsToUse
  fsom$map$medianValues <- t(sapply(seq_len(fsom$map$nNodes),
                                    function(i) {
                                      apply(subset(fsom$data, fsom$map$mapping[, 1] ==
                                                     i), 2, stats::median)
                                    }))
  fsom$map$medianValues[is.nan(fsom$map$medianValues)] <- 0
  colnames(fsom$map$medianValues) <- colnames(fsom$data)
  fsom$map$sdValues <- t(sapply(seq_len(fsom$map$nNodes), function(i) {
    apply(subset(fsom$data, fsom$map$mapping[, 1] == i),
          2, stats::sd)
  }))
  fsom$map$sdValues[is.nan(fsom$map$sdValues)] <- 0
  colnames(fsom$map$sdValues) <- colnames(fsom$data)
  fsom
}

#' Get cluster label for every individual cells
#'
#' @param  fsom             FlowSOM object as generated by the Franken function
#'
#' @return vector label for every cell
#' @examples
#' Define path to directory with your .fcs files
#' DataDir = ''
#' fileName     = list.files(path = DataDir, pattern = "\\.fcs$")
#' readin_files = ReadInput(fileNames,  scale = FALSE)
#' frk_ls       = Franken(readin_files, marker_ids=c(9,12,14:18))
#' cluster_labels = GetClusters(frk_ls)
#'
#' @export
GetClusters <- function(fsom) {

  som_clusters         = fsom$read_aff_SOM$map$mapping[,1]
  som_meta_clusters    = fsom$metaclus$consensus_ids[som_clusters]

  return(som_meta_clusters)
}

