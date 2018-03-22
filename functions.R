# Helper functions

jsButtonColor <- function(var, color, value){
  # Makes simple js code to color buttons in shinyWidgets library. From:
  # https://github.com/dreamRs/shinyWidgets/issues/41
  # var, color and yes/no need to be in quotes
  paste0("$(\"input:radio[name='",
         var,
         "'][value='",
         paste0(value),
         "']\").parent().css('background-color', '",
         color,
         "');")  
}
depSub <- function(a){
  # shortcut helper n
  deparse(substitute(a))
}

evPar <- function(x){
  # short helper fun
  eval(parse(text = x))
}


is.valid.age <- function(x) {
  !is.na(x) &&
    x >= 20 &&
    x <= 99 &&
    x %% 1 == 0
}
