# selenium (development version)

* Added the `browser`, `port` and `host` fields to `SeleniumSession`.
* All web requests now have a timeout by default, which can be customised
  with the `timeout` argument to each method.
* Store the latest release of selenium in an environment, meaning it will
  persist in the current R session.
* Use `gitcreds`, if available, to authenticate GitHub requests.

# selenium 0.1.2

* Add `temp` argument to `selenium_server()`.

# selenium 0.1.1

* Added `path` argument to `selenium_server()`, allowing the file to be
  saved in a custom path.
* Removed `\dontrun{}` for one example for CRAN resubmission.

# selenium 0.1.0

* Initial CRAN submission.
