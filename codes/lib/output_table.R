output_table <- function(data, caption, digits = 3, path, filename, include.rownames = F) {
  tab <- xtable(data, caption = caption, digits = digits)
  
  print(tab, 
        file              = sprintf("%s/%s.tex", path, filename), 
        append            = F, 
        table.placement   = "h",
        caption.placement = "top", 
        hline.after       = seq(from = -1, to = nrow(tab), by = 1), 
        format.args       = list(big.mark = ",", decimal.mark = "."), 
        include.rownames  = include.rownames)
}
