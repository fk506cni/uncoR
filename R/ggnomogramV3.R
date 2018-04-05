#' plot nomogram for estimation of sum of
# this not require sample data
# this plot figure from model info
#' @import ggplot2
#' @import gridExtra
#' @importFrom  magrittr %>%
#' @importFrom  extrafont loadfonts()
#' @param model_info list of model info see example.
#'
#'
which_near <- function(x, comp_vector=c(0.1,0.5,1,2,5,10,20,25,50,100,250,1e3,1e4,1e5,1e6,1e7,1e8)){
  sq2 <- (comp_vector - x)^2
  min_num <- comp_vector[sq2 == min(sq2)]
  return(min(min_num))
}

proper_interval <- function(start, end, divide=3){
  range_num <- end - start
  divide_num <- range_num / divide
  return(which_near(divide_num))
}

proper_segment <- function(start, end, divide=3){
  if(start < end){
    p_int <- proper_interval(start, end, divide)
    #num_quotients <- (end - start) %/% p_int
    intervals <- p_int*(1:(end %/% p_int +1))
    intervals <- intervals[start <= intervals & intervals <= end]
  }else if(start > end){
    p_int <- proper_interval(end, start, divide)
    #num_quotients <- (start - end) %/% p_int
    intervals <- p_int *(1:(start %/% p_int +1))
    intervals <- intervals[end <= intervals & intervals <= start]
    intervals <- intervals[sort.list(intervals, decreasing = T)]
  }

  #this remove intervals in end or start.
  intervals <- intervals[!intervals %in% c(start, end)]

  return(intervals)
}

theme_ggh <- function(font.size=10,legend.position = "none") {
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.spacing = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0,0,0,0),"cm"),
        text = element_text(family = "Arial",size=font.size,lineheight = 0.7),
        legend.position = "none")
}

graph_construct <- function(model_info){
  graph_info <- model_info

  graph_info$y_scale <- list(scale_y_min = 0, scale_y_max = 100)
  graph_info$y_scale$length <- graph_info$y_scale$scale_y_max - graph_info$y_scale$scale_y_min

  ##name
  graph_info$var_name[[length(graph_info$var_name)+1]] <- graph_info$outoput_name

  ##effect range
  graph_info$effect_range <- list()
  for(i in 1:length(graph_info$var_range)){
    graph_info$effect_range[[i]] <- graph_info$betas[[i]] * graph_info$var_range[[i]]
  }

  ##effect length
  graph_info$effect_lengths <- sapply(graph_info$effect_range,
                                      function(arg_vector){return(max(arg_vector)-min(arg_vector))})


  ##V3 position
  v1_x <- 0
  v2_x <- 100
  graph_info$vars_position <- c(v1_x, v2_x,
                                (v2_x - v1_x)*graph_info$effect_lengths[2]/sum(graph_info$effect_lengths[c(1,2)]))

  ##total output range
  graph_info$var_range$Var3 <- c(minimum=sum(sapply(graph_info$effect_range, min)),
                                 maximum=sum(sapply(graph_info$effect_range, max)))

  graph_info$effect_lengths[[3]] <- max(graph_info$var_range$Var3) - min(graph_info$var_range$Var3)


  ##vars length
  graph_info$vars_lengths <- sapply(graph_info$var_range, function(arg){return(max(arg)-min(arg))})

  ##major_str
  major_divide <- 3
  graph_info$major_str <- list()
  for(i in 1:length(graph_info$var_range)){
    graph_info$major_str[[i]] <- proper_segment(start = min(graph_info$var_range[[i]]),
                                                end = max(graph_info$var_range[[i]]),divide = major_divide)
    names(graph_info$major_str)[i] <- names(graph_info$var_name)[i]
  }

  ##major_int
  #graph_info$major_int <- list()
  graph_info$major_int <- list()
  for(i in 1:length(graph_info$var_range)){
    graph_info$major_int[[i]] <- graph_info$y_scale$length * (graph_info$major_str[[i]] - min(graph_info$var_range[[i]])) / graph_info$vars_lengths[[i]]
  }

  ##minor_int
  minor_divide <- 15
  graph_info$minor_str <- list()
  for(i in 1:length(graph_info$var_range)){
    graph_info$minor_str[[i]] <- proper_segment(start = min(graph_info$var_range[[i]]),
                                                end = max(graph_info$var_range[[i]]),divide = minor_divide)
    names(graph_info$minor_str)[i] <- names(graph_info$var_name)[i]
  }

  ##minor_int
  graph_info$minor_int <- list()
  for(i in 1:length(graph_info$var_range)){
    graph_info$minor_int[[i]] <- graph_info$y_scale$length * (graph_info$minor_str[[i]] - min(graph_info$var_range[[i]])) / graph_info$vars_lengths[[i]]
  }

  ##cutoff
  graph_info$cutoff_pos <- c()
  for(i in 1:length(graph_info$cutoff_val)){
    graph_info$cutoff_pos[i] <- graph_info$y_scale$length * (graph_info$cutoff_val[i] - min(graph_info$var_range$Var3)) /graph_info$effect_lengths[3]
  }

  ##labeling sides
  sides_names <- c("end_str","end_int", "major_str","major_int","minor_int","cutoff")
  graph_info$side$Var1 <- c(rep("R", 6))
  graph_info$side$Var2 <- c(rep("L", 6))
  graph_info$side$Var3 <- c("R","B","R","R","R","L")
  for(i in 1:length(graph_info$side)){
    names(graph_info$side[[i]]) <- sides_names
  }

  ##cutoff info
  graph_info$cutoff_onoff <- c(F, F, T)

  ##width_int
  #graph_info$width_int <- c(end_int=1.5, major_int=1, minor_int=0.5)
  graph_info$width_int <- c(end_int=4, major_int=3, minor_int=2)

  return(graph_info)
}

add_ints <- function(p, x, y_ints, int_w, side="B"){
  #both is "B", left is "L", right is "R"
  side_l <- - int_w / 2
  side_r <- int_w / 2
  if(side=="R"){
    side_l <- 0
  }else if(side=="L"){
    side_r <- 0
  }

  for(i in 1:length(y_ints)){
    p <- p+geom_segment(mapping = aes_string(x = x + side_l, xend=x+side_r,
                                             y=y_ints[i], yend=y_ints[i]))
  }
  return(p)
}

add_str <- function(p, x, y_str,y_str_pos, str_w, side="R",str_size=4){
  if(is.null(y_str)){
    y_str <- as.character(y_str_pos)
  }
  if(length(y_str)!=length(y_str_pos)){
    print("different length!")
    break()
  }



  ##this not allow "B" as both sides anntotation
  if(side=="R"){
    side_w <- str_w / 2
    text_hjust <- 0
  }else if(side=="L"){
    side_w <- - str_w / 2
    text_hjust <- 1
  }


  for(i in 1:length(y_str)){
    df_i <- data.frame(x=x + side_w, y=y_str_pos[i], size=str_size, label=y_str[i], hjust=text_hjust)
    p <- p+geom_text(mapping = aes(x=x, y = y,size=size,label=label,hjust=hjust),data = df_i)
  }

  return(p)
}

add_vline <- function(p, x, y, yend){
  p <- p+geom_segment(mapping = aes_string(x = x, xend=x, y = y, yend=yend))
  return(p)
}


plot_vline <- function(p,graph_info,i){
  x <- graph_info$vars_position[i]
  y <- graph_info$y_scale$scale_y_min
  yend <- graph_info$y_scale$scale_y_max
  widths <- graph_info$width_int
  sides <- graph_info$side[i][[1]]

  sides

  end_str <- graph_info$var_range[[i]]
  major_str <- graph_info$major_str[[i]]
  major_int <- graph_info$major_int[[i]]
  minor_int <- graph_info$minor_int[[i]]

  cutoff_bool <- graph_info$cutoff_onoff[i]

  ##main_axis
  p <- add_vline(p = p, x = x,
                 y = y,
                 yend = yend)

  ##ends
  p <- add_ints(p = p,x = x,y_ints = c(y, yend),int_w = widths["end_int"],side = sides["end_int"])

  ##major_int
  p <- add_ints(p=p, x = x, y_ints = major_int,int_w = widths["major_int"],side = sides["major_int"])

  #minor_int
  p <- add_ints(p=p, x = x, y_ints = minor_int,int_w = widths["minor_int"],side = sides["minor_int"] )

  #end str
  p <- add_str(p = p, x=x, y_str = end_str,y_str_pos = c(y, yend),str_w = widths["end_int"],side = sides["end_str"])

  #major_str
  p <- add_str(p = p, x = x, y_str = major_str, y_str_pos = major_int,str_w = widths["end_int"],side = sides["major_str"])

  #minor_str not added


  ##cutoff
  if(cutoff_bool){
    ##cutoff_int
    p <- add_ints(p = p, x = x, y_ints = graph_info$cutoff_pos,int_w = widths["end_int"],side = sides["cutoff"])

    ##cutoff_str
    p <- add_str(p = p, x = x, y_str = graph_info$cutoff_val, y_str_pos = graph_info$cutoff_pos,
                 str_w = widths["end_int"],side = sides["cutoff"])
  }

  return(p)
}

plot_lines <- function(graph_info){
  p <- ggplot()+theme_ggh()

  for(i in 1:3){
    p <- plot_vline(p=p, graph_info = graph_info, i=i)
  }

  return(p)
}

plot_label <- function(p, graph_info, i){
  x <- graph_info$vars_position[i]
  y <- 0
  var_name <- graph_info$var_name[[i]]

  side <- graph_info$side[[i]][1]

  if(side=="R" & i!=3){
    text_hjust <- 0
  }else if(side=="L" & i!=3){
    text_hjust <- 1
  }else if(i==3){
    text_hjust <- 0.5
  }
  print(text_hjust)

  #text_hjust <- c(1,0,0.5)[i]
  df_i <- data.frame(x=x, y=y,label=var_name, hjust=text_hjust)
  p <- p+geom_text(mapping = aes(x = x, y = y, label=label,hjust=hjust),data = df_i,parse = T)
  return(p)
}

plot_formulas <- function(graph_info, formula_h=2){
  p <- ggplot()+theme_ggh()

  ##add name
  for(i in 1:length(graph_info$var_name)){
    p <- plot_label(p = p, graph_info = graph_info, i = i)
  }

  ##add_formulas
  if(!is.null(graph_info$formula)){
    fom <- graph_info$formula
    x <- abs(graph_info$vars_position[[1]] - graph_info$vars_position[[2]])/2
    y <- formula_h
    size <- 7
    df_fom <- data.frame(x=x, y=y, label=fom, size=size)
    p <- p+geom_text(mapping = aes(x = x, y=y, label=label, size=size),data=df_fom, parse=T)

  }
  return(p)
}

offpanel <- function(p){
  require(ggplot2)
  pt <- ggplot_gtable(ggplot_build(p))
  pt$layout$clip[pt$layout$name=="panel"] <- "off"
  return(pt)
}

#' @export
ggnomogramV3 <- function(model_info, add_p =NULL, lmat=NULL, lhei=NULL,lwid=NULL, main="unko"){
  require("magrittr")
  require("ggplot2")
  require(extrafont)
  loadfonts(quiet = T)
  require(gridExtra)
  require(grid)

  blank_grid <- grid.rect(gp=gpar(col="white"))
  add <- blank_grid



  graph_info <- graph_construct(model_info = model_info)

  fomulas <- plot_formulas(graph_info) %>%
    offpanel()
  #fomulas
  v3lines <- plot_lines(graph_info) %>%
    offpanel()
  #v3lines

  if(is.null(lmat) | is.null(lwid) | is.null(lhei)){
    lmat <- rbind(c(1,2,1),
                  c(1,3,1),
                  c(1,4,1),
                  c(1,1,1))
    lwid <- c(0.1, 2, 0.1)
    lhei <- c(0.2,0.5,7,0.1)
  }
  print(lmat)

  g1 <- grid.arrange(blank_grid,#1
                     add,
                     fomulas,
                     v3lines,
                     layout_matrix=lmat,heights=lhei, widths=lwid,
                     top=textGrob(main, gp=gpar(fontsize=20,fontfamily="Arial")))
  result_list <- list(plot_data=g1)
  return(result_list$plot_data)
}


