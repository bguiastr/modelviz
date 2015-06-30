#' Miltefosine pharmacokinetic model parameters
#'
#' A dataset containing the pharmacokinetic typical parameters values and uncertainty
#' for Miltefosine.
#'
#' \itemize{
#'   \item prm. the model parameters typical values: KA (h-1), CL (L/h), V2 (L),
#'   Q (L/h) and V3 (L)
#'   \item rse. the model parameters uncertainty as relative standard error (rse)
#'   \item advan. the nonmem $SUB ADVAN defining a 2-compartment model with
#'   first order absorption
#' }
#'
#' @format A list of 3 levels: prm, rse and advan.
#' @source T. Dorlo et al. Pharmacokinetics of miltefosine in Old World cutaneous
#' leishmaniasis patients. Antimicrob. Agents Chemother. 52:8, 2855–2860. (2008)
#' \url{http://www.ncbi.nlm.nih.gov/pubmed/18519729}
#' @name twocomp
NULL
