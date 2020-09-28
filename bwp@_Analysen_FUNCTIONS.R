######################################################
##Syntax:   FUNCTIONS
##Project:  bwp@ Article
##Owner: IBG / IPK
##Author:    bmc
#R Version: 3.6.1
#R Studio Version 1.2.1335 

##Belongs: bwp@_Analysen_MASTER
######################################################

######################################
#Function for plot table Likert 1-4 

coeff_table <- function(df, ci = .95, ...){
  
  #Get posterior interval 95%
  post <- posterior_interval(df, pars = c("(Intercept)","StudieOberstufe"), prob = ci)
  
  #CRI Intercept
  df_2 <- tibble(term = "StudieLernende", estimate = as.numeric(tidy(df)[1, 2]), std.error = NA, lo_ci = post[1, 1], hi_ci = post[1, 2])
  #Build DF with posterior means and 95% CRI for Study variables
  coeff_sum  <- tidy(df)[1:2, ] %>% 
    cbind(post[1:2, ]) %>% 
    rename(lo_ci = '2.5%', hi_ci = '97.5%') %>%
    select(term, everything()) %>% 
    mutate(estimate = estimate + as.numeric(tidy(df)[1, 2]),
           lo_ci = if_else(lo_ci + as.numeric(tidy(df)[1, 2]) > 1, lo_ci + as.numeric(tidy(df)[1, 2]), 1),
           hi_ci = if_else(hi_ci + as.numeric(tidy(df)[1, 2]) < 4, hi_ci + as.numeric(tidy(df)[1, 2]), 4),
           term = if_else(is.na(term), "StudieLernende", term)) %>% 
    filter(term == "StudieOberstufe") %>% 
    as_tibble()
  bind_rows(df_2, coeff_sum)
  # list(coeff_sum, ref, ref_ci) 
}

#######################
#Line-Break for Labels

addline_format <- function(x,...){
  gsub('y','\n',x)
}


#############
#Plotfunction

plot_bwpat <- function(df, n = 5, s = 1, legend = "none", ...) {
  
  q <- if_else((2 * n + s - 1) >= nrow(df), nrow(df), as.integer((2 * n + s - 1)))
  
  p <- list(a = c(s:q), b = seq(from = s, q, by = 2))
  

  df[p[["a"]],] %>% 
    ggplot(aes(y = estimate, x = as.factor(ranking), color = Studie)) + geom_point(size = 1.7) +
    geom_errorbar(aes(ymin = lo_ci, ymax = hi_ci), width=.6, linetype = "dashed") +
    theme_minimal() +
    theme(legend.position = legend, axis.title.x = element_blank()) +
    labs(y = "Zustimmung") +
    scale_x_discrete(labels = addline_format(df$Variable[p[["b"]]])) +
    scale_color_manual(values = c("#000000", "#E69F00")) +
    ylim(1, 4) +
    theme(legend.title=element_blank())
} 


#Create one DF from listoutput

n_frame_func <- function(df, ...){
  n_frame <- NULL
  for (i in c(1:length(df))){
    i_frame <- df[[i]] %>% 
      mutate(value = names(df[i]),
      )
    n_frame <- bind_rows(n_frame, i_frame)
  }
  return(n_frame)
}