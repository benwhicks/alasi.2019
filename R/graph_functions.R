library(tidygraph)
library(igraph)

# Notes:
# From Maarten's paper: If teachers are visibly present in the learning network, students prefer learning ties with the teacher or facilitator, rather than with peers. (line 740 or so)

# Getting features of subgraphs, by 'forum' field in edges

explore_subgraphs <- function(g) {
    forums <- g %>% 
        activate(edges) %>% 
        pull(forum) %>% 
        unique()
    rec <- forums %>% 
        map_dbl(
            function(x) {
                sg <- g %>% 
                    to_subgraph(forum == x, subset_by = "edges")
                reciprocity(sg$subgraph)
            })
    trans <-  forums %>% 
        map_dbl(
            function(x){
                sg <- g %>% 
                    to_subgraph(forum == x, subset_by = "edges")
                transitivity(sg$subgraph)
            }
        )
        
    return(tibble(forum = forums, reciprocity = rec, transitivity = trans))
}

explore_subgraphs(rdata.psych::gfmBio17)
explore_subgraphs(rdata.psych::gfmBio18)
explore_subgraphs(rdata.psych::gfmSoc17)
explore_subgraphs(rdata.psych::gfmSoc18)

plot_forum_subgraphs <- function(g, ...) {
    fm_rec <- explore_subgraphs(g)
    fm_rec <- fm_rec %>% 
        mutate(facet_title = paste(forum, "\nreciprocity =", round(reciprocity, 2),
                                   "\ntransitivity = ", round(transitivity, 2))) 
    g %>% 
        activate(edges) %>% 
        inner_join(fm_rec, by = "forum") %>% 
        activate(nodes) %>% 
        filter(!node_is_isolated()) %>% 
        ggraph(...) + 
        geom_edge_link(aes(alpha = hits, color = sentiment)) + 
        geom_node_point(aes(color = grade_quartile)) + 
        facet_edges(~facet_title) + 
        theme_graph() +
        theme(legend.position = "none") + 
        viridis::scale_color_viridis(option = "B") +
        scale_edge_color_gradient(low = "red", high = "blue")
}

plot_forum_subgraphs(rdata.psych::gfmBio17, layout = "kk")
plot_forum_subgraphs(rdata.psych::gfmBio18, layout = "kk")
plot_forum_subgraphs(rdata.psych::gfmSoc17, layout = "kk")
plot_forum_subgraphs(rdata.psych::gfmSoc18, layout = "kk")

fm_make_time_graph <- function(fm) {
    fm$from <- as.character(fm$from)
    fm$to <- as.character(fm$to)
    edges <- fm %>% as_tibble() %>% 
        select(subject_code, forum,
               msg_text,
               hits = hit_count, 
               time = posted_date,
               from, to, sentiment, grade_quartile) %>% 
        mutate(to = replace_na(to, "Question"))
    nodes <- data.frame(person = as.character(unique(union(edges$from, edges$to)))) %>% 
        unique() %>% 
        left_join(edges %>% select(person = from, grade_quartile) %>% unique(), by = "person") %>% 
        mutate(v = ifelse(person == "Question", "Q", ifelse(str_detect(person, "Aca"), "T", "S")),
               grade_quartile = replace_na(grade_quartile, 0))
    nodes$v <- factor(nodes$v, levels = c("S", "T", "Q"))
    edges$grade_quartile <- NULL
    tbl_graph(nodes = nodes, 
              edges = edges)
    
}

fm <- fm_make_time_graph(rdata.psych::fmSoc18)

plot_fm_animation <- function(g) {
    g %>% 
        ggraph() + 
        geom_edge_link(aes(color = forum), alpha = 0.4) + 
        geom_node_point(aes(color = grade_quartile, size = as.numeric(v))) + 
        scale_size(range = c(1,3)) +
        viridis::scale_color_viridis(option = "A") +
        theme_graph() + 
        theme(legend.position = "none") + 
        transition_time(time) + 
     #   facet_edges(~ forum) + 
        shadow_wake(0.2)
}

fm_animate <- function(fm) {
    plot_fm_animation(fm_make_time_graph(fm))
}

fm_animate(rdata.psych::fmBio17)
fm_animate(rdata.psych::fmBio18)
fm_animate(rdata.psych::fmBio17)
fm_animate(rdata.psych::fmBio17)