# https://github.com/SeleniumHQ/selenium/blob/trunk/java/src/org/openqa/selenium/Keys.java
#' A list of special keys
#'
#' A named list of special keys, where each key is a single Unicode character,
#' which will be interpreted by selenium as a special key. Each key is just a
#' string, so can be used with string manipulaton functions like [paste()]
#' without any special treatment.
#'
#' @export
keys <- list(
  null = "\ue000",
  cancel = "\ue001", # ^break
  help = "\ue002",
  backspace = "\ue003",
  back_space = "\ue003",
  tab = "\ue004",
  clear = "\ue005",
  return = "\ue006",
  enter = "\ue007",
  shift = "\ue008",
  left_shift = "\ue008",
  control = "\ue009",
  left_control = "\ue009",
  alt = "\ue00a",
  left_alt = "\ue00a",
  pause = "\ue00b",
  escape = "\ue00c",
  space = "\ue00d",
  page_up = "\ue00e",
  page_down = "\ue00f",
  end = "\ue010",
  home = "\ue011",
  left = "\ue012",
  arrow_left = "\ue012",
  up = "\ue013",
  arrow_up = "\ue013",
  right = "\ue014",
  arrow_right = "\ue014",
  down = "\ue015",
  arrow_down = "\ue015",
  insert = "\ue016",
  delete = "\ue017",
  semicolon = "\ue018",
  equals = "\ue019",
  numpad0 = "\ue01a", # number pad keys
  numpad1 = "\ue01b",
  numpad2 = "\ue01c",
  numpad3 = "\ue01d",
  numpad4 = "\ue01e",
  numpad5 = "\ue01f",
  numpad6 = "\ue020",
  numpad7 = "\ue021",
  numpad8 = "\ue022",
  numpad9 = "\ue023",
  multiply = "\ue024",
  add = "\ue025",
  separator = "\ue026",
  subtract = "\ue027",
  decimal = "\ue028",
  divide = "\ue029",
  f1 = "\ue031", # function  keys
  f2 = "\ue032",
  f3 = "\ue033",
  f4 = "\ue034",
  f5 = "\ue035",
  f6 = "\ue036",
  f7 = "\ue037",
  f8 = "\ue038",
  f9 = "\ue039",
  f10 = "\ue03a",
  f11 = "\ue03b",
  f12 = "\ue03c",
  meta = "\ue03d",
  command = "\ue03d",
  zenkaku_hankaku = "\ue040"
)

#' Combine special keys
#'
#' When a chord of keys is passed into `WebElement$send_keys()`, all keys will
#' be pressed in order, and then released at the end. This is simply done by
#' combining the keys into a single string, and appending the NULL key
#' ([keys$null][keys]) to the end. This is useful for keybindings like
#' <Ctrl-V>, where you want the Ctrl key to be released after the action.
#'
#' @param ... The keys to be combined (strings).
#'
#' @examples
#'
#' # <Ctrl-V> will be pressed, then <Ctrl-Alt-V>
#' paste0(
#'   keys$control, "v",
#'   keys$alt, "v"
#' )
#'
#' # <Ctrl-V> will be pressed, then <Alt-V>
#' paste0(
#'   key_chord(keys$control, "v"),
#'   key_chord(keys$alt, "v")
#' )
#'
#' @export
key_chord <- function(...) {
  rlang::check_dots_unnamed()

  paste0(..., keys$null)
}
