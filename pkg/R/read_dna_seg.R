################################################################################
# File reading functions: read dna_seg
################################################################################


# SUPPORT FUNCTIONS FOR READ_DNA_SEG_FROM_GENBANK
# Get start position for a currentFeature Object
get_start <- function(line){
  as.numeric(gsub("[[:blank:]]|[[:alpha:]]|\\(|\\)|\\.\\..*", "", line))
}
get_end <- function(line){
  as.numeric(gsub("[[:blank:]]|[[:alpha:]]|\\(|\\)|.*\\.\\.", "", line))
}
extract_data <- function(extract, cF){
  extract <- gsub(extract, "", grep(extract, cF, value=TRUE))
  if (length(extract) == 0) { extract <- "NA" }
  extract
}

# Read genes from a GenBank file
read_dna_seg_from_file <- function(file, fileType="detect", ...){
  # Import data from file into variable
  importedData <- readLines(file)

  # Find file type
  TYPE <- "Unknown"
  if (fileType == "detect" || fileType == "Detect" || fileType == "DETECT") {
    if (length(grep("^ID", importedData))) {TYPE <- "EMBL"}
    if (length(grep("^LOCUS", importedData))) {TYPE <- "Genbank"}
  }
  if (fileType == "EMBL" || fileType == "embl" || fileType == "Embl") {
    TYPE <- "EMBL"
  }
  if (fileType == "Genbank" || fileType == "GENBANK" || fileType == "genbank") {
    TYPE <- "Genbank"
  }
  if (TYPE == "Unknown") {
    stop("fileType has to be either 'detect', 'embl' or 'genbank'.")
  }
  
  # Extarct and name main segments
  if(TYPE == "Genbank") {
    mainSegments <- grep("^[[:alnum:]]", importedData)
    names(mainSegments) <- gsub("*| .*", "", grep("^[[:alnum:]]", importedData, value=TRUE))
  }

  # SIMPLE ERROR HANDLING
  if(TYPE == "Genbank") {
    if(length(grep("FEATURES|DEFINITION", names(mainSegments))) < 2){
      stop("FEATURES or DEFINITION segment missing in GBK File.")
    }
    if(length(grep("LOCUS", names(mainSegments))) != 1) {
      stop("Number of LOCUS should be 1.")
    }
  }
  
  # Extract META data
  if(TYPE == "EMBL") {
    seg_name <- gsub("^DE[[:blank:]]+", "", grep("^DE", importedData, value=T))
  }
  if(TYPE == "Genbank") {
    seg_name <- gsub("DEFINITION {1,}", "", importedData[mainSegments["DEFINITION"]])
  }
 
  # Extract features only, handles whether FEATURES is the last (or not) entry in GBK file
  if(TYPE == "Genbank") {
    ifelse (which(names(mainSegments) == "FEATURES") == length(mainSegments),
            dataFeatures <- importedData[mainSegments["FEATURES"]:(length(importedData) - 1)],
            dataFeatures <- importedData[mainSegments["FEATURES"]:
            (mainSegments[which(names(mainSegments) == "FEATURES")+1] - 1)])
  }
  if(TYPE == "EMBL") {
    dataFeatures <- grep("^FT", importedData, value=T)
  }

  # SIMPLE ERROR HANDLING
  if(TYPE == "Genbank") {
    if(length(dataFeatures) < 2){ stop("No FEATURES in GBK file.") }
  }
  if(TYPE == "EMBL") {
    if(length(dataFeatures) < 1){ stop("No FEATURES in GBK file.") }
  }

 
  # Extract each start line for each feature
  if(TYPE == "Genbank") {
    startLineOfFeature <- c(1:length(dataFeatures))[- grep("^ {6,}", dataFeatures)]
  }  
  if(TYPE == "EMBL") {
    startLineOfFeature <- grep("FT   [[:alnum:]]", dataFeatures)
  }
  startLineOfFeature <- c(startLineOfFeature, length(dataFeatures))
  

  # Define variables for storage
  nF <- length(startLineOfFeature)-1
  name=character()
  start=numeric()
  end=numeric()
  strand=numeric()
  length=numeric()
  pid=character()
  gene=character()
  synonym=character()
  product=character()
  proteinid=character()
  
  # Loop over all features                     
  for(counter in 1:nF){

    # Get feature, normally 20ish lines... Choses to clean up first after identification of feature.
    currentFeature <- (dataFeatures[ startLineOfFeature[counter] : (startLineOfFeature[counter+1]-1) ])

    # If feature is of GENE type  
    if(length(grep(" gene ", currentFeature)) > 0){
      # If GENE, do something.
    }

    # If feature is of CDS type
    if(length(grep(" CDS ", currentFeature)) > 0){

      # Clean up feature, decreases number of lines etc.
      if(TYPE == "Genbank") {
        currentFeature <- gsub("^ |:|\"| $", "", gsub("[[:blank:]]+|[[:space:]]+",
        " ", strsplit(paste(currentFeature, collapse=""), "   /")[[1]]))  
      }
      if(TYPE == "EMBL") {
        currentFeature <- gsub("^ |:|\"| $", "", gsub("[[:blank:]]+|[[:space:]]+",
        " ", strsplit(paste(gsub("FT", "", currentFeature), collapse=""), "   /")[[1]]))
      }
      
      # Extract gene name or ID
      ifelse(length(grep("gene=", currentFeature)) > 0,
        name <- c(name, extract_data("gene=", currentFeature)),
        name <- c(name, extract_data("locus_tag=", currentFeature)))

      # Extract start and end and length
      start <- c(start, get_start(currentFeature[1]))
      end <- c(end, get_end(currentFeature[1]))
      length <- c(length, (get_end(currentFeature[1]) - get_start(currentFeature[1]) + 1)/3 - 1)

      # Set strand to 1 or -1
      ifelse (length(grep("complement", currentFeature[1])) > 0,
        strand <- c(strand, -1), strand <- c(strand, 1))

      # Extract PID
      if (TYPE == "Genbank") {
        pid <- c(pid, extract_data("db_xref=GI", currentFeature))
      }
      if (TYPE == "EMBL") {
        pidTEMP <- extract_data("db_xref=UniProtKB/Swiss-Prot", currentFeature)
        if (pidTEMP == "NA"){
          pidTEMP <- extract_data("db_xref=UniProtKB/TrEMBL", currentFeature)
          }
        pid <- c(pid, pidTEMP)
      }
      
      # Extract gene
      ifelse(length(grep("gene=", currentFeature)) > 0,
        gene <- c(gene, extract_data("gene=", currentFeature)), gene <- c(gene, "-"))

      # Extract synonym
      synonym <- c(synonym, extract_data("locus_tag=", currentFeature))

      # Extract protein ID
      proteinid <- c(proteinid, extract_data("protein_id=", currentFeature))

      # Extract product
      product <- c(product, extract_data("product=", currentFeature))

      # SIMPLE ERROR HANDLING
      if(is.numeric(start) == FALSE) { stop("Start is not numeric.") }
      if(is.numeric(end) == FALSE) { stop("End is not numeric.") }
      if(is.numeric(length) == FALSE) { stop("Length is not numeric.") }
      if(is.numeric(strand) == FALSE) { stop("Strand is not numeric.") }
      if(is.character(pid) == FALSE) { stop("PID is not character.") }
      if(is.character(name) == FALSE) { stop("Name is not character.") }
      if(is.character(gene) == FALSE) { stop("Gene is not character.") }
      if(is.character(synonym) == FALSE) { stop("Synonym is not character.") }
      if(is.character(product) == FALSE) { stop("Product is not character.") }
      if(is.character(proteinid) == FALSE) { stop("Protein ID is not character.") }
 
      # End of CDS loop
    }

  # End of loop over all features
  }
  
  # Cut table to include only added features
  table <- data.frame(name=name, start=start, end=end, strand=strand,
    length=length, pid=pid, gene=gene, synonym=synonym,
    product=product, proteinid=proteinid, stringsAsFactors=FALSE)

  # Go to next function
  .read_dna_seg(table, seg_name, ...)

  # End of genbank to dna_seg
}



# reading genes from a file. Use source=tab or ptt to specify type
read_dna_seg_from_ptt <- function(file, meta_lines=2, header=TRUE, ...){
  # reads meta info
  seg_name <- readLines(file, n=1)
  seg_name <- strsplit(seg_name, "/,|-/", fixed=TRUE)[[1]][1]
  # reads ptt table
  ptt <- read.table(file, skip=meta_lines, as.is=TRUE, header=header,
                    sep="\t", quote="")
  if (header){
    names(ptt) <- tolower(names(ptt))
  }
  else {
    names(ptt) <- c("location", "strand", "length", "pid", "gene",
                    "synonym", "code", "cog", "product")
  }
  # parse location
  location <- strsplit(ptt$location, "..", fixed=TRUE)
  start <- as.numeric(sapply(location, function(x) x[[1]]))
  end <- as.numeric(sapply(location, function(x) x[[2]]))
  # parse strand
  strand <- ptt$strand
  strand[strand=="-"] <- -1
  strand[strand=="+"] <- 1
  strand <- as.numeric(strand)
  # parse gene name from name or synonym if not present
  name <- ifelse(ptt$gene == "-", ptt$synonym, ptt$gene)
  table <- data.frame(name=name, start=start, end=end, strand=strand,
                      length=ptt$length, pid=ptt$pid, gene=ptt$gene,
                      synonym=ptt$synonym, code=ptt$code, cog=ptt$cog,
                      product=ptt$product,
                      stringsAsFactors=FALSE)
  .read_dna_seg(table, seg_name, ...)
}
read_dna_seg_from_tab <- function(file, header=TRUE, ...) {
  table <- read.table(file, as.is=TRUE, header=header, sep="\t", quote="")
  if (ncol(table) < 4) stop("Insufficent number of columns in table")
  col_names <-  c("name", "start", "end", "strand")
  names(table)[1:length(col_names)] <- col_names
  # parse name from file name by default
  seg_name <- basename(file)
  .read_dna_seg(table, seg_name, ...)
}
.read_dna_seg <- function(table, seg_name, reverse=FALSE, xlim=NULL, ...){
  # check args
  if (ncol(table) < 4) stop("Insufficent number of columns in table")
  if (nrow(table) < 1) stop("No lines in table")
  col_names <-  c("name", "start", "end", "strand")
  if (!all(col_names %in% names(table)))
    stop("Table should contain at least columns name, start, end and strand")
  # make dna_seg object, set seg_name attribute
  dna_seg <- as.dna_seg(table, ...)
  dna_seg <- trim.dna_seg(dna_seg, xlim)
  if (reverse) dna_seg <- reverse.dna_seg(dna_seg)
  attr(dna_seg, "seg_name") <- seg_name
  dna_seg
}
