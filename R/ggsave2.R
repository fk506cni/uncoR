#ggsave2
#wrapper of ggsave2
#this save image by object name and suitable format for publication
#' @export
ggsave2 <- function(plot, wid=9, hei=9, device="tiff"){
  require(ggplot2)
  plot_name <- deparse(substitute(plot))
  file_name <- paste(plot_name, ".",device, sep = "",collapse = "")
  ggsave(filename = file_name,plot = plot,device = device,width = wid, height = hei,dpi = 300,units = "cm")
}
