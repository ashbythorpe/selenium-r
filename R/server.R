# Store the latest version name in an environment, that will persist until
# the R session is closed.
selenium <- rlang::new_environment()

get_selenium_env <- function() {
  utils::getFromNamespace("selenium", ns = "selenium")
}

get_from_env <- function(items) {
  sel_env <- get_selenium_env()
  env_get(sel_env, items, default = NULL)
}

set_in_env <- function(...) {
  sel_env <- get_selenium_env()
  env_bind(sel_env, ...)
}

#' Download and start the Selenium server.
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Downloads the latest release of Selenium Server, and then runs it as a
#' background process. You must have Java installed for this command to work.
#'
#' @param version The version of Selenium Server to download and run. By
#'   default, the latest major or minor release is used.
#' @param selenium_manager Whether to enable Selenium Manager, which will
#'   automatically download any missing drivers. Defaults to `TRUE`.
#' @param interactive By default, if you don't have a version downloaded, you
#'   will be prompted to confirm that you want to download it, and the function
#'   will error if [rlang::is_interactive()] returns `FALSE`. To allow this
#'   function to work in a non-interactive setting, set this to `FALSE`.
#' @param verbose Passed into [utils::download.file()]. Note that setting this
#'   to `FALSE` will *not* disable the prompt if a file needs to be downloaded.
#' @param temp Whether to use a temporary directory to download the Selenium
#'   Server `.jar` file. This will ensure that the file is deleted after it is
#'   used, but means that you will have to redownload the file with every new
#'   R session. If `FALSE`, the file is saved in your user data directory.
#' @param path The path where the downloaded Selenium Server `.jar` file will
#'   be saved. Overrides `temp`.
#' @param echo_cmd Passed into [processx::process$new()][processx::process].
#' @param extra_args A character vector of extra arguments to pass into the
#'   Selenium Server call. See the list of options here:
#'   <https://www.selenium.dev/documentation/grid/configuration/cli_options/>
#'
#' @returns A [processx::process] object. Call `<process>$kill()` to stop the
#'   server.
#'
#' @details
#' This command respects the `JAVA_HOME` environment variable when attempting
#' to find the `java` executable. Otherwise, [Sys.which()] is used.
#'
#' @seealso
#' The [package website](https://ashbythorpe.github.io/selenium-r/index.html)
#' for more ways to start the Selenium server.
#'
#' @examples
#' \dontrun{
#' # Disables the prompt that asks you whether you want to download Selenium server
#' server <- selenium_server(interactive = FALSE)
#'
#' # Saves the server in your user data directory
#' server <- selenium_server(temp = FALSE)
#' server$kill()
#'
#' # The server doesn't have to be downloaded again
#' server <- selenium_server(temp = FALSE)
#'
#' # Here we use extra arguments to increase the timeout of client sessions,
#' # allowing sessions to stay open for longer without being automatically
#' # terminated.
#' server <- selenium_server(extra_args = c("--session-timeout", "3000"))
#' }
#'
#' @export
selenium_server <- function(version = "latest",
                            selenium_manager = TRUE,
                            interactive = TRUE,
                            verbose = TRUE,
                            temp = TRUE,
                            path = NULL,
                            echo_cmd = FALSE,
                            extra_args = c()) {
  check_string(version)
  check_bool(selenium_manager)
  check_bool(interactive)
  check_bool(verbose)
  check_bool(temp)
  check_string(path, allow_null = TRUE)
  check_bool(echo_cmd)
  check_character(extra_args, allow_null = TRUE)

  if (version != "latest") {
    n_version <- numeric_version(version)

    list_version <- unclass(n_version)
    if (list_version[[1]][length(list_version[[1]])] != 0) {
      list_version[[1]][length(list_version[[1]])] <- 0
      class(list_version) <- class(n_version)

      rlang::abort(c(
        "`version` must be a major or minor release, not a patch.",
        "i" = paste0("Supplied version: ", version),
        "i" = paste0("Did you mean: ", list_version, "?")
      ))
    }

    if (n_version < "4.9.0" && selenium_manager) {
      rlang::warn(c(
        "Selenium Server 4.9.0 or higher is required to use Selenium Manager.",
        "x" = paste0("Actual version requested: ", n_version, "."),
        "x" = "Disabling Selenium Manager.",
        "i" = "Set `selenium_manager` to `FALSE` to disable this warning."
      ))
      selenium_manager <- FALSE
    }
  }

  if (version == "latest") {
    release_name <- rlang::try_fetch(get_latest_version_name(), error = identity)
    if (rlang::is_error(release_name)) {
      version <- get_version_from_files(release_name)
      release_name <- paste0("selenium-", version)
    } else {
      version <- gsub("^selenium-", "", release_name)
    }
  } else {
    release_name <- paste0("selenium-", version)
  }

  file_name <- paste0("selenium-server-", version, ".jar")

  if (!is.null(path)) {
    dir <- normalizePath(path, winslash = "/", mustWork = TRUE)
  } else if (temp) {
    dir <- tempfile(pattern = "file", tmpdir = tempdir())
    dir.create(dir)
  } else {
    app_dir <- rappdirs::user_data_dir("selenium-server", "seleniumHQ", version = version)
    dir <- normalizePath(app_dir, winslash = "/", mustWork = FALSE)
  }

  full_path <- if (dir == "") {
    NULL
  } else {
    file.path(dir, file_name)
  }

  if (interactive && !rlang::is_interactive()) {
    rlang::abort(c(
      "An interactive session is required to download Selenium Server.",
      "Set `interactive` to `FALSE` to disable this warning."
    ))
  }

  if ((is.null(full_path) || !file.exists(full_path)) && interactive) {
    choices <- utils::menu(
      title = paste0("Should we download Selenium Server version ", version, " from GitHub?"),
      choices = c("Yes", "No"),
    )
    if (choices != 1) {
      rlang::abort("Selenium Server not found.")
    }
  }

  if (!dir.exists(dir)) {
    dir.create(app_dir, recursive = TRUE)
    dir <- normalizePath(app_dir, winslash = "/", mustWork = FALSE)
    full_path <- file.path(dir, file_name)
  }

  if (!file.exists(full_path)) {
    download_server(full_path, file_name, release_name, verbose)
  }

  args <- c("-jar", full_path, "standalone")

  if (selenium_manager) {
    args <- c(args, "--selenium-manager", "true")
  }
  args <- c(args, extra_args)

  processx::process$new(
    java_check(),
    args = args,
    echo_cmd = echo_cmd,
    supervise = TRUE
  )
}

download_server <- function(path, file, name, verbose) {
  url <- paste0(
    "https://github.com/SeleniumHQ/selenium/releases/download/",
    name,
    "/",
    file
  )

  utils::download.file(url, path, quiet = !verbose)
  file
}

get_latest_version_name <- function() {
  sel_env <- get_selenium_env()

  stored_name <- get_from_env("latest_version_name")
  if (!is.null(stored_name)) {
    return(stored_name)
  }

  req <- httr2::request("https://api.github.com/repos/seleniumHQ/selenium/releases/latest")
  req <- httr2::req_headers(req, "Accept" = "application/vnd.github+json")

  token <- if (is_installed("gitcreds")) {
    tryCatch(
      gitcreds::gitcreds_get(),
      error = function(e) NULL
    )
  } else {
    NULL
  }

  if (!is.null(token)) {
    token <- paste("token", token$password)
    req <- httr2::req_headers(req, Authorization = token)
  }

  response <- httr2::req_perform(req)
  release <- httr2::resp_body_json(response)

  url_parts <- strsplit(release$html_url, split = "/")[[1]]

  url_parts[[length(url_parts)]]
}

get_version_from_files <- function(error) {
  dir <- rappdirs::user_data_dir("selenium-server", "seleniumHQ")
  dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  if (!dir.exists(dir)) {
    rlang::abort("Could not make request to GitHub.", parent = error)
  }

  files <- list.dirs(dir, full.names = FALSE)
  versions <- numeric_version(files[files != ""])
  version <- max((files), na.rm = TRUE)
  rlang::warn(c(
    "Github request failed: Could not determine latest version of Selenium Server.",
    "i" = paste0("Using latest downloaded version ", version, " instead.")
  ))
  version
}

java_check <- function() {
  java_home <- Sys.getenv("JAVA_HOME")
  if (nzchar(java_home)) {
    java <- file.path(java_home, "bin", "java")
    java_windows <- file.path(java_home, "bin", "java.exe")
    if (file.exists(java)) {
      return(java)
    } else if (file.exists(java_windows)) {
      return(java_windows)
    }
  }

  java <- Sys.which("java")
  if (identical(unname(java), "")) {
    rlang::abort("Java not found. Please install Java to use `selenium_server()`.")
  }
  java
}

find_using <- function(x, .f) {
  for (a in x) {
    if (.f(a)) {
      return(a)
    }
  }
  NULL
}

is_nonspecific_release <- function(x) {
  grepl("^selenium-([0-9]+\\.)+0$", x$name)
}
