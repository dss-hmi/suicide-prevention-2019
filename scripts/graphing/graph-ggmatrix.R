# THis script contains function starters and demos for faceting elemental graphs with GGally: http://ggobi.github.io/ggally
# study examples:

#########################################################
### 
#########################################################

g <-  ggplot2::ggplot(d,aes_string(y     = yaxis 
                                   ,x     = "est"
                                   # ,color = "sign"
                                   # ,fill  = "sign"
                                   # ,shape = "model_number"
))  


#  the below example was taken from 
# https://github.com/IALSA/ialsa-2016-time-sensitivity/commit/dbf21c1c34019563e285175a5ebf84ca0d120d0c#diff-d17b8a0709a814a2399c53dd4fa49348
# ---- graphing-functions --------------------------------------------------------------
# define simple plot, to be tiled in a matrix
plot_trajectories <- function(
  d,
  time_var,
  sample_size=100
){
  # dd <- d
  # d <- ds_long
  # time_var = "years_since_bl"
  # # time_var = "wave"
  # sample_size = 100
  # 
  if(!sample_size=="max"){
    set.seed(42)
    ids <- sample(unique(d$id),sample_size)
    dd <- d %>% dplyr::filter(id %in% ids)
  }else{dd <- d}
  # compute sample size
  n_people <- length(unique(dd$id))
  # dd <- d
  g1 <-  dd %>%  
    ggplot2::ggplot(aes_string(x=time_var,y="mmse")) +
    geom_smooth(method="loess", color="black",size=1, fill="black", alpha=.2, linetype="solid", na.rm=T, span=1.5)+
    geom_line(aes(group=id),size=.5,alpha=.06)+
    geom_point(size=2.5, alpha=.4, shape =21)+
    geom_rug(size = 1, sides = "b", alpha = .1)+
    scale_y_continuous(limits = c(-.5,30.5), breaks=seq(-0,30,5))+
    scale_x_continuous(limits = c(0,9), breaks=seq(0,8,2),minor_breaks = seq(0,8,1))+
    geom_text(x=.2, y=1, label = paste0("N = ",scales::comma(n_people)))+
    main_theme+
    theme(text = element_text(size=baseSize+4)) +
    labs(x = "Years since baseline", y = "Mini Mental State Exam")
  return(g1)
}
# usage demo:
# ds_long %>% 
#   plot_trajectories(time_var = "years_since_bl", sample_size = 100) 

# define complext plot, matrix of simple views
matrix_plot <- function(
  d, # ds_long
  patterns
){
  # create a list of plots to facet with ggmatrix
  ls <- list()
  # patterns <- c("1-2-3-.-.", "1-2-3-4-.", "1-2-3-4-5")
  for(pat in patterns){
    ls[[pat]] <- ds_long %>% 
      dplyr::filter(response_pattern == pat) %>% 
      plot_trajectories(time_var = "years_since_bl", sample_size = "max") 
  }
  # place the plots into a single ggmatrix
  mplot <- GGally::ggmatrix(
    ls,
    ncol = 1, nrow = length(patterns),
    title = "Observed MMSE scores for three types of response patterns",
    yAxisLabels = patterns,
    # yAxisLabels = c("0-2-4", "0-2-4-6", "0-2-4-6-8"), 
    xlab = "Years since baseline", ylab = "Mini Mental State Exam (MMSE) Score"
    # xAxisLabels = "MMSE score",
    # legend = 1
  ) + theme(
    legend.position = "right",
    strip.text.x = element_text(size=baseSize+2)
    
  )
  mplot
}
# usage demo:
# ds_long %>% matrix_plot()


# Sonata form report structure
# ---- print-displays ---------------------------------
# print the matrix plot
g <- ds_long %>% 
  matrix_plot( patterns = c("1-2-3-.-.", "1-2-3-4-.", "1-2-3-4-5") )

g %>% 
  quick_save(
    name  = "prints/figure-1"
  )
