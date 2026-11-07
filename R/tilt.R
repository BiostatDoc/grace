tilt_probs <- function(b, winner, xi) {
  if (is.na(winner) || b[[winner]] <= 0 || b[[winner]] >= 1) {
    return(b)
  }
  odds <- b[[winner]] / (1 - b[[winner]]) * xi / (1 - xi)
  p_winner <- odds / (1 + odds)
  tilted <- b * (1 - p_winner) / (1 - b[[winner]])
  tilted[[winner]] <- p_winner
  tilted
}
