% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/element.R
\name{WebElement}
\alias{WebElement}
\title{Create a live element}
\description{
This class represents a single element on the page. It is created using an
existing \link{SeleniumSession} instance.
}
\examples{

## ------------------------------------------------
## Method `WebElement$new`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

element <- session$find_element(using = "css selector", value = "#download")

session$close()
}

## ------------------------------------------------
## Method `WebElement$shadow_root`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

# Let's create our own Shadow Root using JavaScript
session$execute_script("
  const div = document.createElement('div');
  document.body.appendChild(div);
  div.attachShadow({mode: 'open'});
")

element <- session$find_element(using = "css selector", value = "div")

shadow_root <- element$shadow_root()

session$close()
}

## ------------------------------------------------
## Method `WebElement$find_element`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

row <- session$find_element(using = "css selector", value = ".row")

logo_container <- row$find_element(using = "css selector", value = "p")

logo <- logo_container$find_element(using = "css selector", value = "img")

session$close()
}

## ------------------------------------------------
## Method `WebElement$find_elements`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

row <- session$find_element(using = "css selector", value = ".row")

links <- row$find_elements(using = "css selector", value = "a")

session$close()
}

## ------------------------------------------------
## Method `WebElement$is_selected`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "#download")$is_selected()

session$close()
}

## ------------------------------------------------
## Method `WebElement$get_attribute`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$get_attribute("href")

session$close()
}

## ------------------------------------------------
## Method `WebElement$get_property`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$get_property("href")

session$close()
}

## ------------------------------------------------
## Method `WebElement$get_css_value`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$get_css_value("color")

session$close()
}

## ------------------------------------------------
## Method `WebElement$get_text`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "#download")$get_text()

session$close()
}

## ------------------------------------------------
## Method `WebElement$get_tag_name`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "#download")$get_tag_name()

session$close()
}

## ------------------------------------------------
## Method `WebElement$get_rect`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "#download")$get_rect()

session$close()
}

## ------------------------------------------------
## Method `WebElement$is_enabled`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$is_enabled()

session$close()
}

## ------------------------------------------------
## Method `WebElement$computed_role`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$computed_role()

session$close()
}

## ------------------------------------------------
## Method `WebElement$computed_label`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$computed_label()

session$close()
}

## ------------------------------------------------
## Method `WebElement$click`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$click()

session$close()
}

## ------------------------------------------------
## Method `WebElement$clear`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.google.com")

session$find_element(using = "css selector", value = "textarea")$clear()

session$close()
}

## ------------------------------------------------
## Method `WebElement$send_keys`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.google.com")

input <- session$find_element(using = "css selector", value = "textarea")

input$send_keys("Hello")

input$send_keys(key_chord(keys$control, "a"), key_chord(keys$control, "c"))

input$send_keys(keys$control, "v")

input$get_attribute("value")

session$close()
}

## ------------------------------------------------
## Method `WebElement$screenshot`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$screenshot()

session$close()
}

## ------------------------------------------------
## Method `WebElement$is_displayed`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$is_displayed()

session$close()
}

## ------------------------------------------------
## Method `WebElement$toJSON`
## ------------------------------------------------

\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

result <- session$find_element(using = "css selector", value = "a")$toJSON()

result

jsonlite::toJSON(result, auto_unbox = TRUE)

session$close()
}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{id}}{The id of the element, used to uniquely identify it on the
page.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-WebElement-new}{\code{WebElement$new()}}
\item \href{#method-WebElement-shadow_root}{\code{WebElement$shadow_root()}}
\item \href{#method-WebElement-find_element}{\code{WebElement$find_element()}}
\item \href{#method-WebElement-find_elements}{\code{WebElement$find_elements()}}
\item \href{#method-WebElement-is_selected}{\code{WebElement$is_selected()}}
\item \href{#method-WebElement-get_attribute}{\code{WebElement$get_attribute()}}
\item \href{#method-WebElement-get_property}{\code{WebElement$get_property()}}
\item \href{#method-WebElement-get_css_value}{\code{WebElement$get_css_value()}}
\item \href{#method-WebElement-get_text}{\code{WebElement$get_text()}}
\item \href{#method-WebElement-get_tag_name}{\code{WebElement$get_tag_name()}}
\item \href{#method-WebElement-get_rect}{\code{WebElement$get_rect()}}
\item \href{#method-WebElement-is_enabled}{\code{WebElement$is_enabled()}}
\item \href{#method-WebElement-computed_role}{\code{WebElement$computed_role()}}
\item \href{#method-WebElement-computed_label}{\code{WebElement$computed_label()}}
\item \href{#method-WebElement-click}{\code{WebElement$click()}}
\item \href{#method-WebElement-clear}{\code{WebElement$clear()}}
\item \href{#method-WebElement-send_keys}{\code{WebElement$send_keys()}}
\item \href{#method-WebElement-screenshot}{\code{WebElement$screenshot()}}
\item \href{#method-WebElement-is_displayed}{\code{WebElement$is_displayed()}}
\item \href{#method-WebElement-toJSON}{\code{WebElement$toJSON()}}
\item \href{#method-WebElement-clone}{\code{WebElement$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-new"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-new}{}}}
\subsection{Method \code{new()}}{
Initialize a \code{WebElement} object. This should not be called manually:
instead use \link[=SeleniumSession]{SeleniumSession$create_webelement()} if
you have an element id. To find elements on the page, use
\link[=SeleniumSession]{SeleniumSession$find_element()} and
\link[=SeleniumSession]{SeleniumSession$find_elements()}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$new(session_id, req, verbose, id)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{session_id}}{The id of the session that the element belongs to.}

\item{\code{req, verbose}}{Private fields of a \link{SeleniumSession} object.}

\item{\code{id}}{The element id.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{WebElement} object.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

element <- session$find_element(using = "css selector", value = "#download")

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-shadow_root"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-shadow_root}{}}}
\subsection{Method \code{shadow_root()}}{
A shadow DOM is a self-contained DOM tree, contained within another DOM
tree. A shadow root is an element that contains a DOM subtree. This
method gets the shadow root property of an element.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$shadow_root(timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \link{ShadowRoot} object.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

# Let's create our own Shadow Root using JavaScript
session$execute_script("
  const div = document.createElement('div');
  document.body.appendChild(div);
  div.attachShadow({mode: 'open'});
")

element <- session$find_element(using = "css selector", value = "div")

shadow_root <- element$shadow_root()

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-find_element"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-find_element}{}}}
\subsection{Method \code{find_element()}}{
Find the first element matching a selector, relative to the current
element.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$find_element(
  using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
  value,
  request_body = NULL,
  timeout = 20
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{using}}{The type of selector to use.}

\item{\code{value}}{The value of the selector: a string.}

\item{\code{request_body}}{A list of request body parameters to pass to the
Selenium server, overriding the default body of the web request}

\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{WebElement} object.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

row <- session$find_element(using = "css selector", value = ".row")

logo_container <- row$find_element(using = "css selector", value = "p")

logo <- logo_container$find_element(using = "css selector", value = "img")

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-find_elements"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-find_elements}{}}}
\subsection{Method \code{find_elements()}}{
Find all elements matching a selector, relative to the current element.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$find_elements(
  using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
  value,
  request_body = NULL,
  timeout = 20
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{using}}{The type of selector to use.}

\item{\code{value}}{The value of the selector: a string.}

\item{\code{request_body}}{A list of request body parameters to pass to the
Selenium server, overriding the default body of the web request}

\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A list of \code{WebElement} objects.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

row <- session$find_element(using = "css selector", value = ".row")

links <- row$find_elements(using = "css selector", value = "a")

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-is_selected"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-is_selected}{}}}
\subsection{Method \code{is_selected()}}{
Check if an element is currently selected.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$is_selected(timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A boolean value: \code{TRUE} or \code{FALSE}.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "#download")$is_selected()

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-get_attribute"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-get_attribute}{}}}
\subsection{Method \code{get_attribute()}}{
Get an attribute from an element.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$get_attribute(name, request_body = NULL, timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{The name of the attribute.}

\item{\code{request_body}}{A list of request body parameters to pass to the
Selenium server, overriding the default body of the web request}

\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The value of the attribute: a string.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$get_attribute("href")

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-get_property"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-get_property}{}}}
\subsection{Method \code{get_property()}}{
Get a property from an element. Properties are similar to attributes,
but represent the HTML source code of the page, rather than the current
state of the DOM.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$get_property(name, request_body = NULL, timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{The name of the property.}

\item{\code{request_body}}{A list of request body parameters to pass to the
Selenium server, overriding the default body of the web request}

\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The value of the property: a string.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$get_property("href")

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-get_css_value"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-get_css_value}{}}}
\subsection{Method \code{get_css_value()}}{
Get the computed value of a CSS property.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$get_css_value(name, request_body = NULL, timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{The name of the CSS property.}

\item{\code{request_body}}{A list of request body parameters to pass to the
Selenium server, overriding the default body of the web request}

\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The value of the CSS property: a string.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$get_css_value("color")

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-get_text"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-get_text}{}}}
\subsection{Method \code{get_text()}}{
Get the text content of an element.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$get_text(timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The text content of the element: a string.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "#download")$get_text()

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-get_tag_name"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-get_tag_name}{}}}
\subsection{Method \code{get_tag_name()}}{
Get the tag name of an element.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$get_tag_name(timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The tag name of the element: a string.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "#download")$get_tag_name()

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-get_rect"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-get_rect}{}}}
\subsection{Method \code{get_rect()}}{
Get the dimensions and coordinates of an element.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$get_rect(timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A list containing the following elements:
\itemize{
\item \code{x}: The x-coordinate of the element.
\item \code{y}: The y-coordinate of the element.
\item \code{width}: The width of the element in pixels.
\item \code{height}: The height of the element in pixels.
}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "#download")$get_rect()

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-is_enabled"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-is_enabled}{}}}
\subsection{Method \code{is_enabled()}}{
Check if an element is currently enabled.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$is_enabled(timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A boolean value: \code{TRUE} or \code{FALSE}.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$is_enabled()

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-computed_role"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-computed_role}{}}}
\subsection{Method \code{computed_role()}}{
Get the computed role of an element. The role of an element is usually
"generic", but is often used when an elements tag name differs from its
purpose. For example, a link that is "button-like" in nature may have
a "button" role.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$computed_role(timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A string.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$computed_role()

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-computed_label"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-computed_label}{}}}
\subsection{Method \code{computed_label()}}{
Get the computed label of an element (i.e. The text of the label element
that points to the current element).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$computed_label(timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A string.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$computed_label()

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-click"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-click}{}}}
\subsection{Method \code{click()}}{
Click on an element.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$click(timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The element, invisibly.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$click()

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-clear"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-clear}{}}}
\subsection{Method \code{clear()}}{
Clear the contents of a text input element.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$clear(timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The element, invisibly.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.google.com")

session$find_element(using = "css selector", value = "textarea")$clear()

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-send_keys"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-send_keys}{}}}
\subsection{Method \code{send_keys()}}{
Send keys to an element.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$send_keys(..., request_body = NULL, timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{The keys to send (strings). Use \link{keys} for special keys, and
use \code{\link[=key_chord]{key_chord()}} to send keys combinations.}

\item{\code{request_body}}{A list of request body parameters to pass to the
Selenium server, overriding the default body of the web request}

\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The element, invisibly.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.google.com")

input <- session$find_element(using = "css selector", value = "textarea")

input$send_keys("Hello")

input$send_keys(key_chord(keys$control, "a"), key_chord(keys$control, "c"))

input$send_keys(keys$control, "v")

input$get_attribute("value")

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-screenshot"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-screenshot}{}}}
\subsection{Method \code{screenshot()}}{
Take a screenshot of an element.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$screenshot(timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The base64-encoded PNG screenshot, as a string.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$screenshot()

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-is_displayed"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-is_displayed}{}}}
\subsection{Method \code{is_displayed()}}{
Check if an element is displayed. This function may not work on all
platforms.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$is_displayed(timeout = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{timeout}}{How long to wait for a request to receive a response
before throwing an error.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A boolean.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

session$find_element(using = "css selector", value = "a")$is_displayed()

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
Convert an element to JSON. This is used by
\link[=SeleniumSession]{SeleniumSession$execute_script()}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$toJSON()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list, which can then be converted to JSON using
\code{\link[jsonlite:fromJSON]{jsonlite::toJSON()}}.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
session <- SeleniumSession$new()

session$navigate("https://www.r-project.org")

result <- session$find_element(using = "css selector", value = "a")$toJSON()

result

jsonlite::toJSON(result, auto_unbox = TRUE)

session$close()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WebElement-clone"></a>}}
\if{latex}{\out{\hypertarget{method-WebElement-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WebElement$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
