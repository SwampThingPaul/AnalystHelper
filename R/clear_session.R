#' Clear interactive session state (environment, plots, console)
#'
#' Convenience helper for interactive work that removes objects from the global
#' environment, closes open graphics devices, and optionally clears the RStudio
#' console.
#'
#' @details
#' This function is intended for **interactive use** (e.g., at the top of an
#' analysis script). It removes objects from `.GlobalEnv`, closes any open
#' graphics devices via [grDevices::graphics.off()], and clears the console by
#' writing a form-feed character (`"\\014"`), which is interpreted by RStudio as
#' "clear console". This function is purely for internal use and used to ensure
#' scripts are reproducible and enables clearing the environment. However,
#' significant discussion has been had on the subject, providing this for context
#' <https://www.tidyverse.org/articles/2017/12/workflow-vs-script/>
#'
#' Note that clearing the console is IDE-dependent and mainly affects what is
#' *displayed*; it does not reset the session, loaded packages, options, or
#' working directory. It is not recommended to use this function in any package development.
#'
#' @param clear_console Logical; if `TRUE`, attempt to clear the console output
#'   (works in RStudio; may have no effect in other front ends).
#' @param clear_plots Logical; if `TRUE`, close all open graphics devices.
#' @param ... additional arguments to be passed
#'
#' @return Invisibly returns `TRUE` (invisibly) to allow use in pipelines or
#'   script headers without printing.
#'
#' @importFrom beepr beep
#'
#' @keywords internal

clear_session <- function(clear_console = TRUE, clear_plots = TRUE) {
  rm(list = ls(envir = .GlobalEnv, all.names = TRUE), envir = .GlobalEnv)

  if (isTRUE(clear_plots)) grDevices::graphics.off()
  if (isTRUE(clear_console)) cat("\014")

  if (requireNamespace("beepr", quietly = TRUE)) beepr::beep(2)
  message(sprintf("session cleared ... ready to start"))
  invisible(TRUE)
}

