
<!-- README.md is generated from README.Rmd. Please edit that file -->

# selenium

<!-- badges: start -->
<!-- badges: end -->

selenium is a tool for the automation of web browsers. It is a low-level
interface to the [WebDriver](https://w3c.github.io/webdriver/)
specification, and an up-to-date alternative to
[RSelenium](https://github.com/ropensci/RSelenium).

## Installation

You can install the development version of selenium from GitHub:

``` r
pak::pak("ashbythorpe/selenium")
```

However, you must also have a selenium server installed and running (see
below).

## Usage

A selenium instance consists of two parts: the client and the server.
The selenium package *only provides the client*. This means that you
have to start the server yourself.

One of the ways to do this is the following:

1.  Install a browser that you want to automate (e.g. Chrome, Firefox,
    Edge).
2.  Download [Java](https://www.java.com/).
3.  Download the latest `.jar` file for Selenium Server. Do this by
    navigating to the latest GitHub release page
    (<https://github.com/SeleniumHQ/selenium/releases/latest/>),
    scrolling down to the **Assets** section, and downloading the file
    named `selenium-server-standalone-<VERSION>.jar` (with `<VERSION>`
    being the latest release version).
4.  Make sure you are in the same directory as the file you downloaded.
5.  In the terminal, run
    `java -jar selenium-server-standalone-<VERSION>.jar standalone --selenium-manager true`,
    replacing `<VERSION>` with the version number that you downloaded.
    This will download any drivers you need to communicate with the
    server and the browser, and start the server.

There are a few other ways of starting Selenium Server:

- Using docker to start the server. See
  <https://github.com/SeleniumHQ/docker-selenium>. This is recommended
  in a non-interactive context (e.g. GitHub Actions).
- Using the `wdman` package to start the server from R, using
  `wdman::selenium()`. Note that at the time of writing, this package
  does not work with the latest version of Chrome.

Once a server instance has started, move to R and load selenium.

``` r
library(selenium)
```

Client sessions can be started using `SeleniumSession$new()`

``` r
session <- SeleniumSession$new()
```

By default, this will connect to Chrome, but you can use the `browser`
argument to specify a different browser if you like.

``` r
session <- SeleniumSession$new(browser = "firefox")
#> <httr2_request>
#> POST http://localhost:4444/session
#> Headers:
#> • Content-Type: 'application/json; charset=utf-8'
#> • Accept: 'application/json; charset=utf-8'
#> Body: a string
```

If this doesn’t work, you may have to specify the `port` and `host`
arguments manually. If you used the first method above to start the
server, you should have seen a message like this output in the terminal:
`INFO [Standalone.execute] - Started Selenium Standalone ... (revision ...): http://<IP>:<PORT>`
The URL at the end of this message can be used to extract an IP address
and a port number, which can then be passed into the `host` and `port`
arguments. For example, if the URL was: `http://172.17.0.1/4444`, you
would run:

``` r
session <- SeleniumSession$new(host = "172.17.0.1", port = 4444)
```

Once the session has been successfully started, you can use the session
object to control the browser.

``` r
session$navigate("https://www.r-project.org/")
#> <httr2_request>
#> POST http://localhost:4444/session/a07b9550-28a1-4b85-9515-65ab2f848fd2/url
#> Headers:
#> • Content-Type: 'application/json; charset=utf-8'
#> • Accept: 'application/json; charset=utf-8'
#> Body: a string
session$
  find_element(using = "css selector", value = ".row")$
  find_element(using = "css selector", value = "ul")$
  find_element(using = "css selector", value = "a")$
  click()
#> <httr2_request>
#> POST http://localhost:4444/session/a07b9550-28a1-4b85-9515-65ab2f848fd2/element
#> Headers:
#> • Content-Type: 'application/json; charset=utf-8'
#> • Accept: 'application/json; charset=utf-8'
#> Body: a string
#> <httr2_request>
#> POST
#> http://localhost:4444/session/a07b9550-28a1-4b85-9515-65ab2f848fd2/element/ff8f7f31-a5f6-44de-b7cc-91b93bc1c28f/element
#> Headers:
#> • Content-Type: 'application/json; charset=utf-8'
#> • Accept: 'application/json; charset=utf-8'
#> Body: a string
#> <httr2_request>
#> POST
#> http://localhost:4444/session/a07b9550-28a1-4b85-9515-65ab2f848fd2/element/44e21cef-1467-45ec-b532-17910872fd98/element
#> Headers:
#> • Content-Type: 'application/json; charset=utf-8'
#> • Accept: 'application/json; charset=utf-8'
#> Body: a string
#> <httr2_request>
#> POST
#> http://localhost:4444/session/a07b9550-28a1-4b85-9515-65ab2f848fd2/element/c9913f24-7583-49ba-952f-752c3431902e/click
#> Headers:
#> • Content-Type: 'application/json; charset=utf-8'
#> • Accept: 'application/json; charset=utf-8'
#> Body: a string

session$
  find_element(using = "css selector", value = ".row")$
  find_elements(using = "css selector", value = "div")[[2]]$
  find_element(using = "css selector", value = "p")$
  get_text()
#> <httr2_request>
#> POST http://localhost:4444/session/a07b9550-28a1-4b85-9515-65ab2f848fd2/element
#> Headers:
#> • Content-Type: 'application/json; charset=utf-8'
#> • Accept: 'application/json; charset=utf-8'
#> Body: a string
#> <httr2_request>
#> POST
#> http://localhost:4444/session/a07b9550-28a1-4b85-9515-65ab2f848fd2/element/4e7e24cc-da16-4a03-88b6-90b0d9002d6f/elements
#> Headers:
#> • Content-Type: 'application/json; charset=utf-8'
#> • Accept: 'application/json; charset=utf-8'
#> Body: a string
#> <httr2_request>
#> POST
#> http://localhost:4444/session/a07b9550-28a1-4b85-9515-65ab2f848fd2/element/d42ae7f3-9049-49cc-8d66-1eb54448872c/element
#> Headers:
#> • Content-Type: 'application/json; charset=utf-8'
#> • Accept: 'application/json; charset=utf-8'
#> Body: a string
#> <httr2_request>
#> GET
#> http://localhost:4444/session/a07b9550-28a1-4b85-9515-65ab2f848fd2/element/f84c605c-648f-4292-b507-39959d9d602b/text
#> Body: empty
#> [1] ""

session$close()
#> <httr2_request>
#> DELETE http://localhost:4444/session/a07b9550-28a1-4b85-9515-65ab2f848fd2
#> Body: empty
```

For a more detailed introduction to using selenium, see the Getting
Started vignette (`vignette("selenium")`).
<!-- TODO: Make this vignette --->

Note that selenium is low-level and mainly aimed towards developers. If
you are wanting to use browser automation for web scraping or testing,
you may want to take a look at
[selenider](https://github.com/ashbythorpe/selenider) instead.
