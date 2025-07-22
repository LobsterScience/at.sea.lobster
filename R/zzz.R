# startup message

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Welcome to the Bycatch app! ",
                        " Begin data entry by entering input.trip() in this console.
A pdf with At-Sea-Sampling codes can be found in ", system.file("data", package = "Bycatch")))
}
