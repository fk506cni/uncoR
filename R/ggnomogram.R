#' plot nomogram from logistic regression modeling
# this not require sample data
# this plot figure from model info
#' @import ggplot2
#' @import gridExtra
#' @import grid
#' @import magrittr
#' @importFrom  extrafont loadfonts()
#' @param model_info list of model info see example.

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

floor_multi <- function(x, divisor){
  return(floor(x/divisor)*divisor)
}

round_multi <- function(x, divisor){
  return(round(x/divisor)*divisor)
}

ceiling_multi <- function(x, divisor){
  return(ceiling(x/divisor)*divisor)
}

proper_segment <- function(start, end, divide=3){
  if(start < end){
    p_int <- proper_interval(start, end, divide)
    intervals <- seq(floor_multi(start, p_int), floor_multi(end, p_int), p_int)
    intervals <- intervals[start <= intervals & intervals <= end]
  }else if(start > end){
    p_int <- proper_interval(end, start, divide)
    intervals <- seq(round_multi(end, p_int), round_multi(start, p_int), p_int)
    intervals <- intervals[end <= intervals & intervals <= start]
    intervals <- intervals[sort.list(intervals, decreasing = T)]
  }

  #this remove intervals in end or start.
  intervals <- intervals[!intervals %in% c(start, end)]

  return(intervals)
}

logit <- function(prob, inv=F){
  if(inv){
    lgt <- log((1-prob)/prob)
  }else{
    lgt <- log(prob/(1-prob))
  }
  return(lgt)
}

theme_ggh <- function(font.size=10,legend.position = "none", font_family = "Arial") {
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
        text = element_text(family = font_family,size=font.size,lineheight = 0.7),
        legend.position = "none")
}

graph_construct <- function(model_info, text_h =0.5){
  graph_info <- model_info

  if(is.null(graph_info$text_h)){
    graph_info$text_h <- text_h
  }
  graph_info$interval <- 2.5
  graph_info$endsW <- 1
  graph_info$strW <- 1.7
  graph_info$majorW <- 0.7
  graph_info$minorW <- 0.5
  minor_divid <- 9

  graph_info$strSide <- c(point = "L",vars="L", total_point="R", Prob="R")
  graph_info$intSide <- c(point = "L",vars="B", total_point="R", Prob="B")

  scale_min <- 0
  scale_max <- 100

  plot_ylim <- 3
  plot_xlim <- 1.2

  graph_info$point_major_ints <- proper_segment(start = scale_min, end = scale_max)
  graph_info$point_minor_ints <- proper_segment(start = scale_min, end = scale_max,divide = minor_divid)

  graph_info$vars_position <- c(1:(length(graph_info$var_name) -1))
  names(graph_info$vars_position) <- graph_info$var_name[2:length(graph_info$var_name)]

  graph_info$isPositive_beta <- graph_info$betas[c(2:length(graph_info$betas))] > 0
  for(i in 1:length(graph_info$isPositive_beta)){
    if(!graph_info$isPositive_beta[i]){
      graph_info$var_range[[i]] <- c(max(graph_info$var_range[[i]]), min(graph_info$var_range[[i]]))
    }else{
      graph_info$var_range[[i]] <- c(min(graph_info$var_range[[i]]), max(graph_info$var_range[[i]]))
    }
  }

  graph_info$y_scale <- list(scale_y_min = scale_min, scale_y_max = scale_max)
  graph_info$y_scale$length <- graph_info$y_scale$scale_y_max - graph_info$y_scale$scale_y_min

  graph_info$effect_range <- list()
  for(i in 1:length(graph_info$var_range)){
    if(graph_info$betas[[i+1]] > 0){
      graph_info$effect_range[[i]] <- graph_info$betas[[i+1]] * graph_info$var_range[[i]]
    }else if(graph_info$betas[[i+1]] < 0){
      graph_info$effect_range[[i]] <- graph_info$betas[[i+1]] * graph_info$var_range[[i]][c(2,1)]
    }
  }



  graph_info$effect_lengths <- sapply(graph_info$effect_range,
                                      function(arg_vector){return(max(arg_vector)-min(arg_vector))})

  graph_info$total_effect_range <- c(minimum=sum(sapply(graph_info$effect_range, min)),
                                     maximum=sum(sapply(graph_info$effect_range, max)))

  graph_info$total_effect_length <- max(graph_info$total_effect_range) - min(graph_info$total_effect_range)

  graph_info$var_plot_rate <- graph_info$effect_lengths / max(graph_info$effect_lengths)

  graph_info$var_plot_range <- list()
  for(i in 1:length(graph_info$var_plot_rate)){
    graph_info$var_plot_range[[i]] <- graph_info$var_plot_rate[[i]] *c(scale_min, scale_max)
  }

  graph_info$major_ints <- list()
  for (i in 1:length(graph_info$effect_range)) {
    graph_info$major_ints[[i]] <- proper_segment(graph_info$var_range[[i]][[1]], graph_info$var_range[[i]][[2]])
  }

  graph_info$major_ints_pos <- list()
  for(i in 1:length(graph_info$effect_range)){
    graph_info$major_ints_pos[[i]] <- graph_info$var_plot_range[[i]][[2]]*(graph_info$major_ints[[i]] -graph_info$var_range[[i]][[1]]) /(graph_info$var_range[[i]][[2]] - graph_info$var_range[[i]][[1]])
  }


  graph_info$minor_int_bool <- graph_info$var_plot_rate > 0.5


  graph_info$minor_ints <- list()
  for (i in 1:length(graph_info$effect_range)) {
    graph_info$minor_ints[[i]] <- proper_segment(graph_info$var_range[[i]][[1]], graph_info$var_range[[i]][[2]], divide = minor_divid)
  }

  graph_info$minor_ints_pos <- list()
  for(i in 1:length(graph_info$effect_range)){
    graph_info$minor_ints_pos[[i]] <- graph_info$var_plot_range[[i]][[2]]*(graph_info$minor_ints[[i]] -graph_info$var_range[[i]][[1]]) /(graph_info$var_range[[i]][[2]] - graph_info$var_range[[i]][[1]])
  }

  graph_info$total_score_max <- sum(sapply(graph_info$var_plot_range, function(arg){return(max(arg)-min(arg))}))
  graph_info$total_score_min <- sum(sapply(graph_info$var_plot_range, function(arg){return(min(arg))}))
  graph_info$total_score_max_ceil <- ceiling_multi(graph_info$total_score_max,divisor = 50)
  graph_info$total_score_ceil_rate <- graph_info$total_score_max / graph_info$total_score_max_ceil

  graph_info$score_major_int <- proper_segment(graph_info$total_score_min, graph_info$total_score_max_ceil)
  graph_info$score_major_int_pos <- scale_max * graph_info$score_major_int  /graph_info$total_score_max_ceil

  graph_info$score_minor_int <- proper_segment(graph_info$total_score_min, graph_info$total_score_max_ceil,divide = minor_divid)
  graph_info$score_minor_int_pos <- scale_max * graph_info$score_minor_int  /graph_info$total_score_max_ceil

  graph_info$linp <- logit(graph_info$plot_range) - graph_info$betas[1]
  graph_info$linp_pos <- (graph_info$linp - graph_info$total_effect_range[[1]])/graph_info$total_effect_length*  graph_info$y_scale$length*graph_info$total_score_ceil_rate

  #plot margin parameters
  graph_info$score_xlim <- c(0 -plot_xlim, (length(graph_info$var_name)-1)*graph_info$interval +plot_xlim)
  graph_info$pred_xlim <- c(0 -plot_xlim, 1*graph_info$interval +plot_xlim)
  graph_info$score_ylim <- c(scale_min -plot_ylim, scale_max+plot_ylim)
  graph_info$pred_ylim <- c(scale_min -plot_ylim, scale_max+plot_ylim)

  return(graph_info)
}

add_ints <- function(p, x, y_ints, int_w, side="B"){
  #both is "B", left is "L", right is "R"
  #if you cooord flip, upper is "L", lower is "R"
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

  y_str <- as.character(y_str)


  ##this not allow "B" as both sides anntotation
  if(side=="R"){
    side_w <- str_w / 2
    text_hjust <- 1/2
  }else if(side=="L"){
    side_w <- - str_w / 2
    text_hjust <- 1/2
  }


  for(i in 1:length(y_str)){
    df_i <- data.frame(x=x + side_w, y=y_str_pos[i], size=str_size, label=y_str[i], hjust=text_hjust)
    p <- p+geom_text(mapping = aes(x=x, y = y,size=size,label=label,hjust=hjust),data = df_i)
  }

  return(p)
}

plot_vline <- function(p, graph_info, i){
  if(i == 0){
    x <- 0
    y <- graph_info$y_scale$scale_y_min
    y_end <- graph_info$y_scale$scale_y_max

    ends_pos <- c(graph_info$y_scale$scale_y_min, graph_info$y_scale$scale_y_max)
    ends_str <- c(graph_info$y_scale$scale_y_min, graph_info$y_scale$scale_y_max)
    major_ints <- graph_info$point_major_ints
    major_ints_pos <- graph_info$point_major_ints
    major_ints_str <- as.character(graph_info$point_major_ints)
    minor_ints <- graph_info$point_minor_ints
    minor_ints_pos <- graph_info$point_minor_ints
    int_side <- graph_info$intSide["point"]
    str_side <- graph_info$strSide["point"]

  }else{
    x <- graph_info$interval*graph_info$vars_position[[i]]
    y <- graph_info$var_plot_range[[i]][1]
    y_end <- graph_info$var_plot_range[[i]][2]


    ends_pos <- graph_info$var_plot_range[[i]]
    ends_str <- graph_info$var_range[[i]]
    major_ints <- graph_info$major_ints[[i]]
    major_ints_pos <- graph_info$major_ints_pos[[i]]
    minor_ints <- graph_info$minor_ints[[i]]
    minor_ints_pos <- graph_info$minor_ints_pos[[i]]

    int_side <- graph_info$intSide["vars"]
    str_side <- graph_info$strSide["vars"]
  }

  p <- add_vline(p = p, x = x, y = y, y_end = y_end)
  p <- add_ints(p, x, y_ints = ends_pos, int_w = graph_info$endsW,side = int_side)
  p <- add_ints(p, x, y_ints = major_ints_pos,int_w = graph_info$majorW,side = int_side)
  p <- add_ints(p, x, y_ints = minor_ints_pos,int_w = graph_info$minorW,side = int_side)
  p <- add_str(p, x, y_str = ends_str, y_str_pos = ends_pos,
               side = str_side, str_w = graph_info$strW)
  p <- add_str(p, x, y_str = major_ints, y_str_pos = major_ints_pos,
               side = str_side, str_w = graph_info$strW)

  return(p)
}

make_pointpanel <- function(graph_info){
  pp <- ggplot()+theme_ggh()+ scale_x_reverse(limits=graph_info$score_xlim[c(2,1)],expand=c(0,0))
  for(i in 0:length(graph_info$var_range)){
    pp <- plot_vline(p = pp, graph_info = graph_info, i = i)
  }

  pp <- pp +coord_flip()
  return(pp)
}

make_scorepanel <- function(graph_info){
  sp <- ggplot()+theme_ggh() +scale_x_reverse(limits= graph_info$pred_xlim[c(2,1)], expand=c(0,0))

  ##score
  x <- 0
  y <- graph_info$y_scale$scale_y_min
  y_end <- graph_info$y_scale$scale_y_max
  ends_pos <- c(graph_info$y_scale$scale_y_min, graph_info$y_scale$scale_y_max)
  ends_str <- as.character(c(graph_info$total_score_min, graph_info$total_score_max_ceil))
  major_ints <- graph_info$score_major_int
  major_ints_pos <- graph_info$score_major_int_pos
  major_ints_str <- as.character(graph_info$score_major_int)
  minor_ints <- graph_info$score_minor_int
  minor_ints_pos <- graph_info$score_minor_int_pos
  int_side <- graph_info$intSide["total_point"]
  str_side <- graph_info$strSide["total_point"]

  sp <- add_vline(p = sp, x = x, y = y, y_end = y_end)

  sp <- add_ints(p = sp, x = x, y_ints = ends_pos,int_w = graph_info$endsW,
                 side = graph_info$intSide["total_point"])
  sp <- add_ints(p = sp, x = x, y_ints = major_ints_pos, int_w = graph_info$majorW,
                 side = graph_info$intSide["total_point"])
  sp <- add_ints(p = sp, x = x, y_ints = minor_ints_pos, int_w = graph_info$minorW,
                 side = graph_info$intSide["total_point"])

  sp <- add_str(p = sp, x = x, y_str = ends_str, y_str_pos = ends_pos,
                str_w = graph_info$strW,side = graph_info$strSide["total_point"])
  sp <- add_str(p = sp, x = x, y_str = major_ints_str, y_str_pos = major_ints_pos,
                str_w = graph_info$strW,side = graph_info$strSide["total_point"])


  ##pred
  x <- 1*graph_info$interval
  y <- min(graph_info$linp_pos)
  y_end <- max(graph_info$linp_pos)

  major_ints <- graph_info$linp
  major_ints_pos <- graph_info$linp_pos
  major_ints_str <- as.character(graph_info$plot_range)

  sp <- add_vline(p = sp, x = x, y = y, y_end = y_end)
  sp <- add_ints(p = sp, x = x, y_ints = major_ints_pos, int_w = graph_info$majorW,
                 side = graph_info$intSide["Prob"])
  sp <- add_str(p = sp, x = x, y_str = major_ints_str, y_str_pos = major_ints_pos,
                str_w = graph_info$strW,side = graph_info$strSide["Prob"])


  sp <- sp+coord_flip()
  return(sp)
}

plot_labels <- function(graph_info, tag, label_size=6){
  vars_interval <- graph_info$interval
  ##tag is "score" or "predict"
  if(tag=="score"){
    left_labels <- c("'Point'",graph_info$var_name[2:length(model_info$var_name)])
    y_lim <- -graph_info$score_xlim
  }else if(tag=="predict" & model_info$model_type=="logistic"){
    left_labels <- c("'Total points'", "'Probability'")
    y_lim <- -graph_info$pred_xlim
  }

  p <- ggplot()+theme_ggh()+scale_y_continuous(limits=y_lim[c(2,1)], expand=c(0,0))
  for(i in 1:length(left_labels)){
    label_i <- data.frame(x=0, y=-(i-1)*vars_interval,
                          label=left_labels[i],size=label_size)
    p <- p+geom_text(data = label_i ,
                     mapping = aes(x=x, y=y, label=label,size=size),parse = T)

  }
  return(p)
}

offpanel <- function(p){
  require(ggplot2)
  pt <- ggplot_gtable(ggplot_build(p))
  pt$layout$clip[pt$layout$name=="panel"] <- "off"
  return(pt)
}

make_nomogram <- function(graph_info, vars_interval=7, add_p=NULL, lmat=NULL, lhei=NULL, lwid=NULL,  text_h=1.5){
  require(ggplot2)
  require(gridExtra)
  require(grid)
  require(magrittr)
  require(extrafont)
  loadfonts(quiet = T)

  main  <- graph_info$title


  blank_grid <- grid.rect(gp=gpar(col="white"))

  if(!is.null(add_p)){
    add <- add_p
  }else{
    add <- blank_grid
  }

  text_h <- graph_info$text_h
  score_h <- vars_interval*length(model_info$betas) - text_h
  prep_h <- vars_interval*2

  # scop
  scop <- make_pointpanel(graph_info)
  scop <- scop+scale_y_continuous(expand=c(0,0), limits = graph_info$score_ylim)
  scop
  scop <- offpanel(scop)

  prep <- make_scorepanel(graph_info)
  prep <- prep+scale_y_continuous(expand=c(0,0),limits = graph_info$pred_ylim)
  prep <- offpanel(prep)
  # prep

  # score_l
  score_l <- plot_labels(graph_info,tag = "score")
  #score_l <- score_l+scale_y_continuous(expand=c(0,0),limits = graph_info$score_xlim)
  score_l <- offpanel(score_l)

  # prep_l
  prep_l <- plot_labels(graph_info,tag = "predict")
  #prep_l <- prep_l+scale_y_continuous(expand=c(0,0),limits = graph_info$pred_xlim)
  prep_l <- offpanel(prep_l)


  if(is.null(lmat) | is.null(lwid) | is.null(lhei)){
    lmat <- rbind(c(1,2,1,1),
                  c(1,5,3,1),
                  c(1,1,1,1),
                  c(1,6,4,1),
                  c(1,1,1,1))
    lwid <- c(0.001, 0.2, 1, 0.001)
    lhei <- c(0.3,8,0.01,5,0.001)
  }
  #print(lmat)

  g1 <- grid.arrange(blank_grid,#1
                     add,#2
                     scop,#3
                     prep,#4
                     score_l,#5
                     prep_l,#6
                     layout_matrix=lmat,heights=lhei, widths=lwid,
                     top=textGrob(main, gp=gpar(fontsize=16,fontfamily="Arial")))

  result_list <- list(plot_data=g1)
  return(result_list)
}



#' @export
#'
ggnomogram <- function(model_info){
  graph_info <- graph_construct(model_info = model_info)
  g1 <- make_nomogram(graph_info = graph_info)
  return(g1$plot_data)
}

#' @export
#'
ggsave2 <- function(plot, wid=9, hei=9){
  plot_name <- deparse(substitute(plot))
  file_name <- paste(plot_name, ".tiff", sep = "",collapse = "")
  ggsave(filename = file_name,plot = plot,device = "tiff",width = wid, height = hei,dpi = 300,units = "cm")
}


