
logit.inv = function(x) exp(x)/(1+exp(x))

de_factor <- function(df){
  df %>% dplyr::mutate_if(is.factor, as.character) -> df
}

# Remove rare -1's from df
neg1to0 <- function(df){
  df %>% 
    mutate_if(
      is.numeric, function(x) ifelse(x == -1, 0, x) # Remove -1's
    )
}

# loadings plot
logisticPCA_loadings_plot <- function(m, data){
  df <- data.frame(m$U)
  df$variable <- names(data)
  p1 <-
    ggplot(df, aes(X1, fct_reorder(variable, X1), colour=X1)) +
    ggalt::geom_lollipop(horizontal = T, size = 1, show.legend = FALSE) +
    scale_color_gradient2(low = 'red', mid = 'white', 'high' = 'blue', name = 'Loading') +
    theme_bw(15) +
    labs(title = "PC 1", x = "\nLoading", y = "")
  p2 <-
    ggplot(df, aes(X2, fct_reorder(variable, X2), colour=X2)) +
    ggalt::geom_lollipop(horizontal = T, size = 1, show.legend = FALSE) +
    scale_color_gradient2(low = 'red', mid = 'white', 'high' = 'blue', name = 'Loading') +
    theme_bw(15) +
    labs(title = "PC 2", x = "\nLoading", y = "")
  p1 + p2
}