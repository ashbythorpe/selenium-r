## Resubmission
This is a resubmission responding to the review comments.

The response to the original submission (0.1.0) stated:
"Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir().
e.g.: R/server.R"

The response to the previous submission stated:
"The path argument in selenium_server() is perfectly fine, but it would
be appreciated if the function would not write per default into the user
data directory"

I have updated `selenium_server()` to use a tempfile by default.

I have updated `selenium_server()` to include a `path` argument. The example has been updated to use a temporary
directory to store the Selenium server file. The function uses the user data directory (`rappdirs::user_data_dir()`)
to store the file by default.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
