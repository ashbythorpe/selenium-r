# selenium 0.1.4

- Added `chrome_options()`, `firefox_options()` and `edge_options()` to help
  with the `capabilities` argument in `SeleniumSession$new()`. The documentation
  there includes several links that document the options available for each
  browser, along with a few examples.
- The `error` argument in `wait_for_selenium_available()` now defaults to
  `TRUE`. This means that the function will throw an error if a Selenium server
  is not available by default.
- Added `wait_for_server()`, a version of `wait_for_selenium_available()` that
  gives more detailed error messages by reading the logs of a server process
  created using `selenium_server()`.
- Fixed a bug in `selenium_server()` where fetching the latest version
  didn't work when Selenium was preparing to release a new version.

# selenium 0.1.3

- The `browser`, `host` and `port` fields can now be used to access the browser,
  host and port of a `SeleniumSession` object.
- All web requests now have a 20 second timeout by default, meaning that if a
  response is not received within 20 seconds, an error is thrown. This stops
  requests running indefinitely, and can be customised with the `timeout`
  argument to each method.
- Previously, `selenium_server()` could error if called too many times with
  `version = "latest"`. This is because a GitHub request is made to access
  the latest version, which can cause GitHub's rate limits to be exceeded.
  This update takes two steps to stop this from happening:
  - The latest version, when fetched, is cached in the current R session,
    allowing it to be re-used.
  - `gitcreds` is used, if available, to authenticate GitHub requests,
    increasing the rate limit.
- `selenium_server()` passes in `"|"` to `stdout` and `stderr`, instead of
  exposing them as user arguments. This allows the output/error to be read
  using `$read_output()` and `$read_error()`.

# selenium 0.1.2

- Add `temp` argument to `selenium_server()`.

# selenium 0.1.1

- Added `path` argument to `selenium_server()`, allowing the file to be
  saved in a custom path.
- Removed `\dontrun{}` for one example for CRAN resubmission.

# selenium 0.1.0

- Initial CRAN submission.
