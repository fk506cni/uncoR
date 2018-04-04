## ggnomogram

~~~
require(devtools)
install_github("fk506cni/uncoR")
require("fk506cni/uncoR")

model_info <- list()
model_info$title <- "Test nomogram"
model_info$model_type <- "logistic"
model_info$var_name <- c("'constant'", "'Age'","'Alb'(g/dL)","'Height'(cm)","'CEA'(u/L)")
model_info$betas <- c(-5.901, 0.02, 85*0.01, 0.035, -0.2)
model_info$var_range <- list(Var1=c(0, 50), Var2=c(2, 5), Var3=c(100, 170),Var4=c(0,10))
model_info$plot_range <- c(0.1,0.2,0.5,0.8,0.975)


gtest <- ggnomogram(model_info = model_info)

ggsave2(gtest,wid = 18, hei = 18, device="png")
~~~

![nomo](https://github.com/fk506cni/uncoR/blob/master/gtest.png)
