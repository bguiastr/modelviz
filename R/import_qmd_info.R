#' Import model parameters
#'
#' @description Import model parameters from NONMEM run
#'
#' @param dir location of the model files
#' @param runno run number to be evaluated
#' @param prefix prefix of the model file name
#' @param ext model file extention
#'
#' @seealso \code{\link{format_qmd_info}}, \code{\link{qmd}}
#' @return A list containing the parameters and RSE as \code{"data.frame"},
#'         the nonmem ADVAN (advan) and TRANS (trans) as numeric value.
#' @examples
#' \dontrun{
#' prm_list <- import_qmd_info(dir='../models/pk/', runno='001')
#' }
#' @export
import_qmd_info <- function(dir=NULL, prefix='run', runno, ext='.mod', file=NULL){
  if(!is.null(file)) {
    file_full <- file
  } else {
    file_full <- paste0(dir, '/', prefix, runno, ext)
  }
  if(!file.exists(file_full)) {
    stop(paste0("Sorry, file ", file_full, " could not be found."))
  }
  tmp     <- readLines(file_full)
  subr    <- tmp[grep('$SUB',tmp,fixed=T)]
  subr    <- as.numeric(sapply(unlist(strsplit(subr,'\\s+'))[-1],gsub,pattern='\\D',replacement=''))

  # get $DES
  sel_des <- grep("\\$DES", tmp)
  sel_dollar <- grep("\\$", tmp)
  des_info <- NULL
  if(length(sel_des) > 0) {
    sel_end_des[(sel_dollar - sel_des) > 0][1] - 1
    des_block <- tmp[sel_des:sel_end_des]
    des_info <- NULL
    if(is.null(des_block)) {
      des_info <- parse_des_block(des_block)
    }
  }

  tmp2     <- sapply(strsplit(tmp[grep('.*FILE\\s*=\\s*patab.*',tmp, perl=TRUE)],'.*FILE\\s*=\\s*',perl=TRUE),'[',2)
  tmp2     <- tmp2[file.exists(paste0(dir, tmp2))]
  tmp2     <- do.call('cbind',lapply(paste0(dir, tmp2), read.table, skip=1, header=T, as.is=T))

  if(!'ID' %in% colnames(tmp2)) {
    stop('Need the ID column in parameter table')
  } else {
    prm <- tmp2[!duplicated(tmp2[,'ID']), !duplicated(colnames(tmp2)) &
                 !colnames(tmp2)%in%c('DV','PRED','RES','WRES') &
                 grepl('^(?!(ETA|ET)\\d)',colnames(tmp2), perl=TRUE)] # Remove IIV for now

    out <- list(prm = prm,
                rse = NA,
                advan = subr[1],
                trans = subr[2],
                des_info = des_info
                )
    return(out)
  }
}
