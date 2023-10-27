## Resubmission
This is a resubmission responding to the review comments.

The response to the original submission stated:
"\dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user.
Does not seem necessary for all.
Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}."

I have got rid of `\dontrun{}` for one example (`R/server.R`). However, all other examples that make use of
`dontrun{}` require Selenium server to be installed, and for an instance to be running, and so should not be run
by default.

The response also said:
"Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir().
e.g.: R/server.R"

I have updated `selenium_server()` to include a `path` argument. The example has been updated to use a temporary
directory to store the Selenium server file. The function uses the user data directory (`rappdirs::user_data_dir()`)
to store the file by default.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
