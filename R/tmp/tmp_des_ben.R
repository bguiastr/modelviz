library(stringr)

# In import_qmd_info
qmd_info       <- skeleton_qmd_info(help = FALSE)

## considering adding parsed model in qmd_info
qmd_info$model <- parse_nonmem_model(dir = 'inst/models/', runno = '201')
qmd_info$advan <- 13
qmd_info$trans <- 1

# In define_comp_layout
if(!qmd_info$advan %in% c(1:4, 11:12)) {
  if(is.null(qmd_info$model)) {
    stop('Model code required in \"qmd_info\" when $DES used.')
  }

  label  <- qmd_info$model$CODE[qmd_info$model$ABREV=='MOD']

  if(all(grepl('COMP=', label, fixed = TRUE))){
    label  <- gsub('^.+\\(|,.*$|\\).*$','',label)
    label  <- paste0(toupper(substr(label,1,1)),tolower(substr(label,2,nchar(label))))
  }else if(any(grepl('NCOMP.*=', label))){
    label  <- as.numeric(gsub('.+=\\s*','',label[grepl('NCOMP.*=', label)]))
    label  <- paste('Comp',1:label)
  } else {
    # Look into $DES?
  }

  node <- DiagrammeR::create_nodes(nodes = paste0('A', 1:(length(label)+1)),
                                   label = c(label, 'Output'),
                                   rank  = c(1:length(label),
                                             ifelse(any(grepl('Central',label)),
                                                    grep('Central',label), 0)),
                                   prm   = NA,
                                   rse   = NA)
  ## end here

}


