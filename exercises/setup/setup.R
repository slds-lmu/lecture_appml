if (!exists("show.solution")) {
  show.solution = T
}

knitr::knit_hooks$set(optipng = knitr::hook_optipng)
# Just some preparation
knitr::opts_chunk$set(
  cache = FALSE,
  warning = FALSE,
  message = FALSE,
  echo = TRUE,
  collapse = TRUE,
  optipng = "",
  R.options = list(width = 120, scipen = 100)
)

lgr::get_logger("mlr3")$set_threshold("warn")
# SET BBOTK TO 'info' IF YOU PLAY AROUND WITH THIS!
lgr::get_logger("bbotk")$set_threshold("info")
