round_near <- function(x) {
  if(abs(x) <1) {
    y = round(abs(x)+0.05,1)
  } else if (abs(x) <1000) {
    y = round(abs(x)+5,-1)
  } else if (abs(x) <100000) {
    y = round(abs(x)+50,-2)
  } else if (abs(x) <10000000) {
    y = round(abs(x)+500,-3)
  } else {
    y = round(abs(x)+5000,-4)
  }
  if (x <0 ) {
    -y
  } else {
    y
  }
}
