#' Import model parameters
#'
#' @description Import model parameters from NONMEM run
#'
#' @param dir where are the model files located
#' @param runno run number of the table files to read
#' @param prefix start of the model file name
#' @param ext model file extention
#' @seealso \code{\link{prm_format}}, \code{\link{modelviz}}
#' @return A list containing the parameters and RSE as \code{"data.frame"},
#'         the nonmem ADVAN (advan) and TRANS (trans) as numeric value.
#' @examples
#' \dontrun{
#' prm_list <- prm_import(dir='../models/pk/', runno='001')
#' }
#' @export
prm_import  <- function(dir=NULL,prefix='run',runno,ext='.mod'){

  tmp     <- readLines(paste0(dir,prefix,runno,ext))
  subr    <- tmp[grep('$SUB',tmp,fixed=T)]
  subr    <- as.numeric(sapply(unlist(strsplit(subr,'\\s+'))[-1],gsub,pattern='\\D',replacement=''))
  tmp     <- sapply(strsplit(tmp[grep('.*FILE\\s*=\\s*patab.*',tmp,perl=TRUE)],'.*FILE\\s*=\\s*',perl=TRUE),'[',2)
  tmp     <- tmp[file.exists(paste0(dir,tmp))]
  tmp     <- do.call('cbind',lapply(paste0(dir,tmp),read.table,skip=1,header=T,as.is=T))
  if(!'ID'%in%colnames(tmp)){stop('Need the ID column in patab')
  }else{
    tmp <- tmp[!duplicated(tmp[,'ID']), !duplicated(colnames(tmp)) &
                 !colnames(tmp)%in%c('DV','PRED','RES','WRES') &
                 grepl('^(?!(ETA|ET)\\d)',colnames(tmp),perl=TRUE)] # Remove IIV for now
    tmp <- list(prm=tmp,rse=NA,advan=subr[1],trans=subr[2])
    return(tmp)
  }
}
