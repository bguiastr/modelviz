#' Parse $DES
#'
#' @description Parse a $DES block from NONMEM
#'
#' @param des_block string containing the $DES block from NONMEM
#' @seealso \code{\link{import_qmd_info}}, \code{\link{qmd}}
#' @return A \code{list}
#' @export

parse_des_block = function(des_block = NULL) {
  remove_empty <- function(x) {
    if(class(x) == "list") {
      lapply(x, function(y) { remove_empty(y) })
    } else {
      x[x!=""]
    }
  }
  tmp <- remove_empty(str_split(str_replace_all(des_block, " ", ""), "\\n")[[1]])
  tmp <- tmp[tmp!=""]
  dadt <- tmp[!is.na(str_match(tmp, "DADT\\(.?\\)"))]
  par_transl <- tmp[is.na(str_match(tmp, "DADT\\(.?\\)"))]
  des_info <- str_replace_all(dadt, "DADT\\(.?\\)=","")
  return(des_info)
}
