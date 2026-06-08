#' Deprecated reader aliases
#'
#' @description These wrappers are kept for backward compatibility and will be
#'   removed in a future release. The reader API is now a single function per
#'   format that opens a dialog when its path is `NULL` and detects folders
#'   automatically: [read_trikinetics()], [read_clocklab()], [read_csv_data()],
#'   [read_satellite()], [read_vitalpatch()].
#'
#' @param file,directory Path forwarded to the replacement function.
#' @param ... Arguments forwarded to the replacement function.
#' @return The value of the replacement function.
#' @name circadiandynamics-deprecated
#' @keywords internal
NULL

#' @rdname circadiandynamics-deprecated
#' @export
read_trikinetics_nested <- function(file = NULL) {
  .Deprecated("read_trikinetics(layout = \"nested\")")
  read_trikinetics(file, "nested")
}

#' @rdname circadiandynamics-deprecated
#' @export
read_trikinetics_long <- function(file = NULL) {
  .Deprecated("read_trikinetics(layout = \"long\")")
  read_trikinetics(file, "long")
}

#' @rdname circadiandynamics-deprecated
#' @export
read_trikinetics_folder_nested <- function(directory = NULL) {
  .Deprecated("read_trikinetics(layout = \"nested\")")
  read_trikinetics(directory, "nested")
}

#' @rdname circadiandynamics-deprecated
#' @export
read_trikinetics_folder_long <- function(directory = NULL) {
  .Deprecated("read_trikinetics(layout = \"long\")")
  read_trikinetics(directory, "long")
}

#' @rdname circadiandynamics-deprecated
#' @export
read_trikinetics_nested_interactive <- function() read_trikinetics(file.choose(), "nested")

#' @rdname circadiandynamics-deprecated
#' @export
read_trikinetics_long_interactive <- function() read_trikinetics(file.choose(), "long")

#' @rdname circadiandynamics-deprecated
#' @export
read_trikinetics_folder_nested_interactive <- function() read_trikinetics(rstudioapi::selectDirectory(), "nested")

#' @rdname circadiandynamics-deprecated
#' @export
read_trikinetics_folder_long_interactive <- function() read_trikinetics(rstudioapi::selectDirectory(), "long")

#' @rdname circadiandynamics-deprecated
#' @export
read_clocklab_folder <- function(directory = NULL) {
  .Deprecated("read_clocklab")
  read_clocklab(directory)
}

#' @rdname circadiandynamics-deprecated
#' @export
read_clocklab_interactive <- function() read_clocklab(file.choose())

#' @rdname circadiandynamics-deprecated
#' @export
read_clocklab_folder_interactive <- function() read_clocklab(rstudioapi::selectDirectory())

#' @rdname circadiandynamics-deprecated
#' @export
read_csv_data_interactive <- function() read_csv_data(file.choose())

#' @rdname circadiandynamics-deprecated
#' @export
read_satellite_interactive <- function() read_satellite(file.choose())

#' @rdname circadiandynamics-deprecated
#' @export
read_vitalpatch_interactive <- function() read_vitalpatch(rstudioapi::selectDirectory())

# --- renamed analysis functions (dotted names were never real S3 methods) ---

#' @rdname circadiandynamics-deprecated
#' @export
analyze_timeseries.acf <- function(...) { .Deprecated("analyze_acf"); analyze_acf(...) }

#' @rdname circadiandynamics-deprecated
#' @export
analyze_timeseries.cosinor <- function(...) { .Deprecated("analyze_cosinor"); analyze_cosinor(...) }

#' @rdname circadiandynamics-deprecated
#' @export
analyze_timeseries.lomb <- function(...) { .Deprecated("analyze_lomb"); analyze_lomb(...) }

#' @rdname circadiandynamics-deprecated
#' @export
process_timeseries.main <- function(...) { .Deprecated("process_timeseries_main"); process_timeseries_main(...) }

#' @rdname circadiandynamics-deprecated
#' @export
process_timeseries.core <- function(...) { .Deprecated("process_timeseries_core"); process_timeseries_core(...) }

#' @rdname circadiandynamics-deprecated
#' @export
process_timeseries.waveform <- function(...) { .Deprecated("process_timeseries_waveform"); process_timeseries_waveform(...) }
