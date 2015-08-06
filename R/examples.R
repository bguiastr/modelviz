#' QMD Examples
#'
#' @description A dataset containing the pharmacokinetic typical parameters values
#' and uncertainty for nevirapine (\code{onecomp}), miltefosine (\code{twocomp}) and
#' ciclosporin (\code{threecomp}).
#'
#' @details
#' The \code{examples} file contains the following:
#' \itemize{
#' \item \code{examples$onecomp} nevirapine model information for QMD
#' \item \code{examples$twocomp} miltefosine model information for QMD
#' \item \code{examples$threecomp} ciclosporin model information for QMD
#' }
#'
#' Each of these three examples contain the following:
#' \itemize{
#'   \item prm. the model parameters typical values eg. KA (h-1), CL (L/h) and V (L)
#'   \item rse. the model parameters uncertainty as relative standard error (rse)
#'   \item advan. the nonmem subroutine ADVAN defining the structural model
#' }
#'
#' @format A list of 3 levels: \code{onecomp}, \code{twocomp} and \code{threecomp}
#'
#' @source \href{http://www.ncbi.nlm.nih.gov/pubmed/18751690}{onecomp}: D. Elsherbiny et al.
#' Population pharmacokinetics of nevirapine in combination with rifampicin-based short
#' course chemotherapy in HIV- and tuberculosis-infected South African patients.
#' Eur J Clin Pharmacol. 65:71–80. (2009)
#'
#' @source \href{http://www.ncbi.nlm.nih.gov/pubmed/18519729}{twocomp}: T. Dorlo et al.
#' Pharmacokinetics of miltefosine in Old World cutaneous leishmaniasis patients.
#' Antimicrob. Agents Chemother. 52:8, 2855–2860. (2008)
#'
#' @source \href{http://www.ncbi.nlm.nih.gov/pubmed/17662086}{threecomp}: S. Fanta et al.
#' Developmental pharmacokinetics of ciclosporin – a population pharmacokinetic study
#' in paediatric renal transplant candidates. British Journal of Clinical Pharmacology.
#' 64:6, 772–784. (2007)
#'
#' @examples
#' # One-compartment model
#' qmd(examples$onecomp)
#'
#' # Two-compartment model
#' qmd(examples$twocomp)
#'
#' # Three-compartment model
#' qmd(examples$threecomp)
#'
#' @name examples
NULL
