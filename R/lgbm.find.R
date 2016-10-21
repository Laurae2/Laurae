#' Find LightGBM Path
#'
#' This function opens a file browser which lets you select manually LightGBM.
#' 
#' @return The path of the selected file which can be used for LightGBM (\code{lgbm_path} parameter).
#' 
#' @examples
#' \dontrun{
#' lgbm.find()
#' }
#' 
#' @export

lgbm.find <- function() {file.choose()}