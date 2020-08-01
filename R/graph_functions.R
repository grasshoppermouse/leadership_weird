

binary_dist <- function(d, method='binary'){
  mat <- as.matrix(dist(t(d), method = method))
  colnames(mat) <- rownames(mat) <- names(d)
  return(mat)
}

mstedge <- function(d, method = 'binary'){
  if (method == 'binary'){
    dst <- as.matrix(dist(t(d), method = 'binary'))
    colnames(dst) <- names(d)
    rownames(dst) <- names(d)
  } else if (method == 'mi') {
    dst <- exp(-mutinformation(d))
  }
  g <- graph_from_adjacency_matrix(dst, mode = 'undirected', weighted = T, diag = F)
  m <- mst(g, algorithm = 'prim')
  el <- as_edgelist(m)
  apply(el, 1, function(x) paste0(x, collapse = '*')) # hash edges
}

bootedges <- function(d, nboot=1000, method = 'binary'){
  edges <- character()
  n = nrow(d)
  for (i in 1:nboot){
    if (! i %% 100) print(i)
    indices <- sample(1:n, n, replace = T)
    edges <- c(edges, mstedge(d[indices,], method=method))
  }
  tbl <- 100*table(edges)/nboot
  df <- 
    tibble(edge = names(tbl), bootval = as.numeric(unname(tbl))) %>% 
    separate(edge, into = c('from', 'to'), sep = '\\*')
  dst <- binary_dist(d, method=method)
  df$weight <- map2_dbl(df$from, df$to, ~ dst[.x, .y])
  return(df)
}

mstgraph <- function(g, layout = 'nicely', weight='weight', size=NULL){
  p <- ggraph(g, layout) + 
    geom_edge_link(aes_string(width=weight, color='weight'), alpha=0.75) +
    geom_node_text(aes(label = name), repel = T) +
    scale_edge_width('Resamples (%)') + 
    # scale_edge_alpha('Resamples (%)') + 
    scale_size('Cultures (%)') + 
    scale_edge_color_viridis('Binary distance', option = 'C') +
    labs(title = 'Minimum spanning tree') +
    theme_graph()
  if (is.null(size)){
    p <- p + geom_node_point()
  } else {
    p <- p + geom_node_point(aes_string(size = size))
  }
  return(p)
}

add_edge_var <- function(g, df, var){
  g <- as_tbl_graph(g)
  nodes <- g %>% activate(nodes) %>% as_tibble()
  edges <- g %>% activate(edges) %>% as_tibble()
  edges$from <- nodes$name[edges$from] # Assumes edges are numeric
  edges$to <- nodes$name[edges$to]
  var_dict <- df[[var]]
  names(var_dict) <- paste(df$from, df$to)
  edges[var] <- var_dict[paste(edges$from, edges$to)]
  return(graph_from_data_frame(edges, vertices = nodes, directed = F))
}

cult_support_dict <- function(d1, d2){
  d <- bind_rows(d1, d2)
  dict <- 100*d$Estimate
  names(dict) <- d$Variable
  return(dict)
}
