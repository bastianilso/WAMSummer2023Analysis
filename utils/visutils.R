# p_lin: Predict Linear
# Returns a 100 row (x,y) data frame, which describe a line.
p_lin <- function(df, response, term) {
  min = min(df[[term]]) * 100
  max = max(df[[term]]) * 100
  fm <- lm(as.formula(paste(response, "~", term)), data = df)
  df_p <- data.frame(x = (min:max)/100)
  df_p[[term]] <- (min:max)/100
  curve <- data.frame(x = (min:max)/100, y = predict(fm, df_p))
  return(curve)
}


p_supsmu <- function(df, response, term, b=0.1) {
  curve = smooth.spline(df[[term]], df[[response]], spar=b)
  return(tibble(x = curve$x, y=curve$y))
}

n_clip <- function(x, a = 0, b = 1) {
  ifelse(x <= a,  a, ifelse(x >= b, b, x))
}

t_color <- function(x, levels, colors) {
  #debug
  #x = c(0.23, 0.45, 0.95, 0.01)
  #l = tibble(levels = c(0, 0.2,0.55,0.85,1.1),
  #           colors = c("g0","g1", "g2", "g3", "g4"))
  #levels = l$levels
  #colors = l$colors
    
  toMatch = sapply(x, function(x) min(levels[levels>=x]))
  
  match = match(toMatch, levels)
  c <- colors[match]
  
  return(c)
}
