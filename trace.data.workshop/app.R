#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# library(lakit) - not on CRAN so will not load. Can install via github benwhicks/lakit
library(lubridate)
library(plotly)
# Open Uni data set
# activity <- read_csv(file.path('anonymisedData', 'studentVle.csv'))
# courses <- read_csv(file.path('anonymisedData', 'courses.csv'))
# assessments <- read_csv(file.path('anonymisedData', 'assessments.csv'))
# marks <- read_csv(file.path('anonymisedData', 'studentAssessment.csv'))
# student_rego <- read_csv(file.path('anonymisedData', 'studentRegistration.csv'))
# student_info <- read_csv(file.path('anonymisedData', 'studentInfo.csv'))
# vle <- read_csv(file.path('anonymisedData', 'vle.csv'))

#S-ACC331_201930_W_D - - want to get forum, grades
aa <- read_csv(file.path('data','mock_aa.csv'))
cc <- read_csv(file.path('data','mock_cc.csv'))
#fm
gb <- read_csv(file.path('data', 'mock_gb.csv'))


as_duration_approx <- function(s) {
    d <- as.duration(s) %>% 
        as.character() %>% 
        str_remove("^.* \\(") %>% 
        str_remove("\\)$")
    return(d)
}

timelist_to_difference <-  function(timestamps, frac = 1) {
    n <- length(timestamps)
    if (n < 2) {
        return(lubridate::as.duration(NA))
    }
    M <- matrix(rep(as.numeric(timestamps), n), ncol = n, nrow = n)
    M_diff <- M - t(M)
    diff <- abs(M_diff[upper.tri(M_diff, diag = FALSE)])
    if (min(diff) == 0) {
        next_min <- min(diff[diff > 0])
        diff <- c(diff[diff > 0], rep(next_min * 0.5, length(diff[diff == 0])))
    }
    if (frac < 1) {
        n <- max(floor(length(diff)*frac), 1L)
        diff <- sample(diff, size = n, replace = TRUE)
    }
    output <- lubridate::as.duration(diff)
    return(output)
}

is_numericable <- function(x) {
    suppressWarnings(all(!is.na(as.numeric(x))))
}

analyse_single_student_activity <- function(x) {
    # Needs person, timestamp, session fields
    # person should be unique
    person <- unique(x$person)
    if (length(person) != 1) stop("Only 1 person")
    intervals <- x %>% 
        group_by(session) %>% 
        nest() %>% 
        mutate(intervals = map(data, 
                               ~timelist_to_difference(.$timestamp))) %>% 
        select(-data) %>% 
        unnest(intervals) %>% 
        pull(intervals)
    int_df <- data.frame(person = person,
                         intervals = intervals) %>% 
        as_tibble() 
    # %>% 
    #     filter(!is.na(intervals)) %>% 
    #     filter(intervals < as.duration(60*60*24))
    accesses <- length(unique(x$session))
    clicks_pa <- x %>% 
        group_by(session) %>% 
        tally() %>% 
        pull(n) %>% 
        mean(na.rm = T)
    clicks_sd <- x %>% 
        group_by(session) %>% 
        tally() %>% 
        pull(n) %>% 
        sd(na.rm = T)
    
    # Calculating durations
    time <- max(x$timestamp) - min(x$timestamp)
    time_md <- x %>% 
        group_by(session) %>% 
        summarise(time = max(timestamp) - min(timestamp)) %>% 
        pull(time) %>% 
        median()
    time_sd <- x %>% 
        group_by(session) %>% 
        summarise(time = max(timestamp) - min(timestamp)) %>% 
        pull(time) %>% 
        sd(na.rm = T) %>% 
        as.duration()
    timestamp_mean <- mean(x$timestamp)
    timestamp_median <- median(x$timestamp)
    
    return(list(features = tibble(person = person,
                                  accesses = accesses,
                                  clicks_pa = clicks_pa,
                                  clicks_sd = clicks_sd,
                                  time = time,
                                  time_md = time_md,
                                  time_sd = time_sd,
                                  timestamp_mean = timestamp_mean,
                                  timestamp_median = timestamp_median),
                intervals = int_df))
}

map_student_activity_to_df <- function(x) {
    map_dfr(.x = unique(x$person),
            .f = ~analyse_single_student_activity(x %>% filter(person == .x))$features)
}

map_student_activity_to_intervals <- function(x) {
    map_dfr(.x = unique(x$person),
            .f = ~analyse_single_student_activity(
                x %>% 
                    filter(person == .x)
            )$intervals)
}

map_student_activity_to_cluster_df <- function(x) {
    M <- x %>% 
        select(-person) %>% 
        mutate_all(as.numeric) %>% 
        scale() %>% 
        replace_na(0)
    df <- tibble(
        person = x$person,
        cluster_2 = cluster::pam(M, 2)$cluster,
        cluster_3 = cluster::pam(M, 3)$cluster,
        cluster_4 = cluster::pam(M, 4)$cluster,
        cluster_5 = cluster::pam(M, 5)$cluster,
        dim1 = princomp(M)$scores[, 1],
        dim2 = princomp(M)$scores[, 2],
        dim3 = princomp(M)$scores[, 3]
    )
    return(df)
}

map_content_to_intervals <- function(aa, cc, filter = TRUE) {
    x <- inner_join(aa, cc, by = "pk1") %>% 
        select(person, timestamp, type)
    intervals <- x %>% 
        group_by(type, person) %>% 
        nest() %>% 
        mutate(intervals = map(data, 
                               ~timelist_to_difference(.$timestamp))) %>% 
        select(-data) %>% 
        unnest(intervals) 
    if (filter) {
        intervals <- intervals %>% 
            filter(intervals < as.duration(60*60*24))
    }
    return(intervals) 
}

plot_timestamp_spectrum <- function(df, trans = 'log10', group = NULL, color = NULL, lower_x_lim = 0.1, ...) {
    # This function could use some optimisation
    if(!("intervals" %in% names(df))) stop("No column named 'intervals' in data frame")
    df$intervals = as.numeric(df$intervals) # Histogram didn't like date types
    
    df <- df %>%
        filter(intervals > 0) # ignoring 0 duration events
    
    g <- ggplot(data = df, aes(x = intervals)) +
        geom_density(aes(group = !!enquo(group), color = !!enquo(color)), ...) +
        scale_x_continuous(name = "Interval",
                           limits = c(lower_x_lim, NA),
                           trans = trans,
                           breaks = c(0.1,1,10,60, 600, 60*60, 60*60*24, 60*60*24*7, 60*60*24*7*52/12.0, 60*60*24*7*26),
                           labels = c("0.1s","1s", "10s", "1m", "10m", "1h", "1d", "1w", "1M", "6M")) +
        scale_y_continuous(name = "Amplitude") +
        theme_minimal() +
        theme(axis.line.y = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())
    return(g)
}

# Student lens
DFint <- map_student_activity_to_intervals(aa)
DFsum <- map_student_activity_to_df(aa)
DFclust <- map_student_activity_to_cluster_df(DFsum)
DFint <- left_join(DFint, 
                   DFclust %>% 
                       select(person, starts_with("cluster")),
                   by = "person")
DFsum <- left_join(DFsum, 
                   DFclust %>% 
                       select(person, starts_with("cluster")),
                   by = "person")

# Bring in grade, post clustering
DFsum <- DFsum %>% 
    inner_join(gb, by = "person") %>% 
    select(person, grade, everything())
DFclust <- DFclust %>% 
    inner_join(gb, by = "person")

# Content lens
DFcontint <- map_content_to_intervals(aa, cc)
DFcontint <- left_join(DFcontint, 
                       DFclust %>% 
                           select(person, starts_with('cluster')),
                       by = "person")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exploring trace data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("student",
                        "Choose student",
                        choices = c("All", unique(DFint$person))
            ),
            radioButtons("k", "Cluster k: ", c(2,3,4,5))
        )
        ,
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Student", 
                                 plotOutput("intervalPlot"),
                                 tableOutput("tableSummary")),
                        tabPanel("Cluster",
                                 plotOutput("clusterPlot2d"),
                                 plotlyOutput("clusterPlot3d"),
                                 tableOutput("clusterSummary"),
                                 plotOutput("clusterSpectrums")
                                 ),
                        tabPanel("Content",
                                 plotOutput("contentSpectrums"))
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$intervalPlot <- renderPlot({
        data <- DFint
        if (input$student == "All") {
            plot_timestamp_spectrum(data)
        } else {
            plot_timestamp_spectrum(data %>% filter(person == input$student))
        }

    })
    
    output$tableSummary <- renderTable({
        if (input$student == "All") {
            map_df(DFsum %>% select(-person, -starts_with("cluster")), 
                   ~mean(., na.rm = T))  %>% 
                mutate_at(c("time", "time_md", "time_sd"), as_duration_approx) %>%
                mutate_at(c("timestamp_mean", "timestamp_median"),
                          ~as.character(as_datetime(.)))
        } else {
            DFsum %>% 
                filter(person == input$student) %>% 
                select(-starts_with("cluster")) %>% 
                mutate_at(c("time", "time_md", "time_sd"), as_duration_approx) %>%
                mutate_at(c("timestamp_mean", "timestamp_median"),
                          ~as.character(as_datetime(.)))
        }
    })
    
    output$clusterSummary <- renderTable({
        if (input$k == 2) {
            DFsum %>% 
                group_by(cluster_2) %>% 
                summarise_at(c("grade", "accesses", "clicks_pa", "clicks_sd",
                               "time", "time_md", "time_sd", 
                               "timestamp_mean", "timestamp_median"),
                             ~mean(., na.rm = T)) %>% 
                mutate_at(c("time", "time_md", "time_sd"), as_duration_approx) %>%
                mutate_at(c("timestamp_mean", "timestamp_median"),
                          ~as.character(as_datetime(.)))
        } else {
            if (input$k == 3) {
                DFsum %>% 
                    group_by(cluster_3) %>% 
                    summarise_at(c("grade", "accesses", "clicks_pa", "clicks_sd",
                                   "time", "time_md", "time_sd", 
                                   "timestamp_mean", "timestamp_median"),
                                 ~mean(., na.rm = T)) %>% 
                    mutate_at(c("time", "time_md", "time_sd"), as_duration_approx) %>%
                    mutate_at(c("timestamp_mean", "timestamp_median"),
                              ~as.character(as_datetime(.)))
            } else {
                if (input$k == 4) {
                    DFsum %>% 
                        group_by(cluster_4) %>% 
                        summarise_at(c("grade", "accesses", "clicks_pa", "clicks_sd",
                                       "time", "time_md", "time_sd", 
                                       "timestamp_mean", "timestamp_median"),
                                     ~mean(., na.rm = T)) %>% 
                        mutate_at(c("time", "time_md", "time_sd"), as_duration_approx) %>%
                        mutate_at(c("timestamp_mean", "timestamp_median"),
                                  ~as.character(as_datetime(.)))
                } else {
                    DFsum %>% 
                        group_by(cluster_5) %>% 
                        summarise_at(c("grade", "accesses", "clicks_pa", "clicks_sd",
                                       "time", "time_md", "time_sd", 
                                       "timestamp_mean", "timestamp_median"),
                                     ~mean(., na.rm = T)) %>% 
                        mutate_at(c("time", "time_md", "time_sd"), as_duration_approx) %>%
                        mutate_at(c("timestamp_mean", "timestamp_median"),
                                  ~as.character(as_datetime(.)))
                }
            }
        }
    })
    
    output$clusterSpectrums <- renderPlot({
        # This is horrible code but it should work
        if (input$k == 2) {
            plot_timestamp_spectrum(DFint, 
                                    group = factor(cluster_2),
                                    color = factor(cluster_2)) +
                colorspace::scale_colour_discrete_qualitative()
        } else {
            if (input$k == 3) {
            plot_timestamp_spectrum(DFint, 
                                    group = factor(cluster_3),
                                    color = factor(cluster_3)) +
                colorspace::scale_colour_discrete_qualitative()
            } else {
                if (input$k == 4) {
            plot_timestamp_spectrum(DFint, 
                                    group = factor(cluster_4),
                                    color = factor(cluster_4)) +
                colorspace::scale_colour_discrete_qualitative()
                } else {
                    plot_timestamp_spectrum(DFint, 
                                            group = factor(cluster_5),
                                            color = factor(cluster_5)) +
                        colorspace::scale_colour_discrete_qualitative()
                }
            }
        }
    })
    
    output$contentSpectrums <- renderPlot({
        # This is horrible code but it should work
        if (input$k == 2) {
            plot_timestamp_spectrum(DFcontint, 
                                    group = factor(cluster_2),
                                    color = factor(cluster_2)) +
                colorspace::scale_colour_discrete_qualitative() +
                facet_wrap(~type, scales = "free_y")
        } else {
            if (input$k == 3) {
                plot_timestamp_spectrum(DFcontint, 
                                        group = factor(cluster_3),
                                        color = factor(cluster_3)) +
                    colorspace::scale_colour_discrete_qualitative()+
                    facet_wrap(~type, scales = "free_y")
            } else {
                if (input$k == 4) {
                    plot_timestamp_spectrum(DFcontint, 
                                            group = factor(cluster_4),
                                            color = factor(cluster_4)) +
                        colorspace::scale_colour_discrete_qualitative()+
                        facet_wrap(~type, scales = "free_y")
                } else {
                    plot_timestamp_spectrum(DFcontint, 
                                            group = factor(cluster_5),
                                            color = factor(cluster_5)) +
                        colorspace::scale_colour_discrete_qualitative()+
                        facet_wrap(~type, scales = "free_y")
                }
            }
        }
    })
    
    output$clusterPlot2d <- renderPlot({
        # This is horrible code but it should work
        if (input$k == 2) {
            DFclust %>% 
                ggplot(aes(x = dim1, y = dim2, color = factor(cluster_2))) +
                geom_point(aes(size = grade)) +
                colorspace::scale_colour_discrete_qualitative() +
                theme_minimal() 
        } else {
            if (input$k == 3) {
                DFclust %>% 
                    ggplot(aes(x = dim1, y = dim2, color = factor(cluster_3))) +
                    geom_point(aes(size = grade)) +
                    colorspace::scale_colour_discrete_qualitative() +
                    theme_minimal()
            } else {
                if (input$k == 4) {
                    DFclust %>% 
                        ggplot(aes(x = dim1, y = dim2, color = factor(cluster_4))) +
                        geom_point(aes(size = grade)) +
                        colorspace::scale_colour_discrete_qualitative() +
                        theme_minimal()
                } else {
                    DFclust %>% 
                        ggplot(aes(x = dim1, y = dim2, color = factor(cluster_5))) +
                        geom_point(aes(size = grade)) +
                        colorspace::scale_colour_discrete_qualitative() +
                        theme_minimal()
                }
            }
        }
        
    })
    
    output$clusterPlot3d <- renderPlotly({
        # This is horrible code but it should work
        if (input$k == 2) {
            plot_ly(DFclust, x = ~dim1, y = ~dim2, z = ~dim3,
                    color = ~factor(cluster_2), 
                    size = ~grade,
                    sizes = c(50,300),
                    type = "scatter3d", mode = "markers")
        } else {
            if (input$k == 3) {
                plot_ly(DFclust, x = ~dim1, y = ~dim2, z = ~dim3,
                        color = ~factor(cluster_3), 
                        size = ~grade,
                        sizes = c(50,300),
                        type = "scatter3d", mode = "markers")
            } else {
                if (input$k == 4) {
                    plot_ly(DFclust, x = ~dim1, y = ~dim2, z = ~dim3,
                            color = ~factor(cluster_4),
                            size = ~grade,
                            sizes = c(50,300),
                            type = "scatter3d", mode = "markers")
                } else {
                    plot_ly(DFclust, x = ~dim1, y = ~dim2, z = ~dim3,
                            color = ~factor(cluster_5),
                            size = ~grade,
                            sizes = c(50,300),
                            type = "scatter3d", mode = "markers")
                }
            }
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
