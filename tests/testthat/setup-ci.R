if (identical(Sys.getenv("CI", "false"), "true")) {
  wait_for_selenium_available(5 * 60)
}
