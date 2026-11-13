pdf("figures/tradeoff.pdf", width = 6.3, height = 5.6)
symbols <- c(srs = 1, blocks = 2, minimization = 15, msb = 4, mmsb = 19)
op <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 0.5), cex.main = 0.9)
for (cell in cells) {
  x <- S[[cell]]$guess$mean
  y <- S[[cell]]$smd_end$mean
  plot(
    x, y,
    pch = symbols[method_order], cex = 1.3, xlim = c(46, 76),
    ylim = c(0, max(y) * 1.2), xlab = "Correct-guess probability (%)",
    ylab = "Mean largest SMD at end", main = cell_label[[cell]], las = 1, bty = "l"
  )
  label_position <- c(srs = 4, blocks = 1, minimization = 4, msb = 1, mmsb = 3)
  text(x, y,
    labels = method_label[method_order], pos = label_position[method_order],
    cex = 0.6, offset = 0.6
  )
}
par(op)
invisible(dev.off())
