#load necessary packages
library(shiny) #run the app
library(jsonlite) #read in JSON
library(tidyverse) #data manipulation, ggplot2, etc.
library(scales) #nice formatting
theme_set(theme_bw()) #set theme for ggplot2
#library(lme4) #possibly fixed mixed effects models
library(stringr) #easy string maniupulation
library(DT) #interactive data tables
library(RColorBrewer) #color palettes
#data files from github
simp_three_by_three <-
"https://raw.githubusercontent.com/uwgraphics/cs765-dc3-data-and-code/master/SimpleData/3x3.json"
  
simp_twenty_by_ten <- 
"https://raw.githubusercontent.com/uwgraphics/cs765-dc3-data-and-code/master/SimpleData/20x10.json"

real_fifty_by_fifteen <-
  "https://raw.githubusercontent.com/uwgraphics/cs765-dc3-data-and-code/master/RealisticData/50x15.json"

real_oneH_by_fifteen <- 
  "https://raw.githubusercontent.com/uwgraphics/cs765-dc3-data-and-code/master/RealisticData/100x15.json"

real_838 <-
  "https://raw.githubusercontent.com/uwgraphics/cs765-dc3-data-and-code/master/RealisticData/765-old-anon.json"

# summarise a numeric vector
summary_fun <- function(x, name){
  vec <- c('max' = max(x),
           'min' = min(x),
           'median' = median(x),
           'mean' = mean(x),
           'sum' = sum(x),
           'count' = length(x))
  
  names(vec) <- paste0(name, '_', names(vec))
  vec
}

#scales and transformations for axes
auto_scale <- function(axis = 'x', scale){
  if(axis == 'x'){
    if(scale == 'log2'){
      scale_x_continuous(trans = log_trans(2))
    }else if(scale == 'sqrt'){
      scale_x_continuous(trans = sqrt_trans())
    }else{
      scale_x_continuous()
    }
  }else{
    if(scale == 'log2'){
      scale_y_continuous(trans = log_trans(2))
    }else if(scale == 'sqrt'){
      scale_y_continuous(trans = sqrt_trans())
    }else{
      scale_y_continuous()
    }
  }
}

#helper function to avoid typing
#similar to ISNULL in T-SQL
replace_NULL <- function(x){
  as.numeric(ifelse(is.null(x), NA, x))
}

##clean up the data so that it's in a tidy format
##each row is one assignment, including summaries of post length
get_post_data <- function(js, num_order = 'row_num'){
  #get number of assignments and students
  num_assignments <- length(js$assignments)
  num_students <- length(js$students)
  #get student names
  student_names <- sapply(1:num_students, FUN = function(s) 
    js$students[[s]]$sortable_name)
  #get student ids
  student_id <- sapply(1:num_students, FUN = function(s) 
    js$students[[s]]$id)
  #get assignment type
  #if the length of the assignment slots is > 1, we can probably
  #pull of a prompted field
  if(length(js$assignments[[1]]) > 1){
    assign_names <- sapply(1:num_assignments, 
                           function(a) js$assignments[[a]]$name)
    
    assign_prompts <- sapply(1:num_assignments,
                             function(a) js$assignments[[a]]$prompted)
  }else{
    assign_names <- sapply(1:num_assignments,
                           function(a) js$assignments[[a]])
    
    assign_prompts <- rep(NA, length(assign_names))
  }
  #fix assignment prompts if we couldn't find it
  if(is.null(unlist(assign_prompts))){
    assign_prompts <- rep(NA, length(assign_names))
  }
  
  #type <- trimws(gsub('\\d', '', assign_names))
  #text matching to determine the appropriate assignment types
  type <- ifelse(
    str_detect(assign_names, c('Seek', 'Find')),
    'Seek & Find',
    ifelse(str_detect(assign_names, c('Reading')),
           'Reading',
           ifelse(str_detect(assign_names, c('Design Challenge')),
                  'Design Challenge',
                  ifelse(str_detect(assign_names, '838-Only'),
                         '838-Only',
                         ifelse(str_detect(assign_names, c('Disc')),
                                'Discussion', 'Other')
                         
                  )
           )
    )
  )
  
  if(num_order == 'row_num'){
    
    assign_number <- 
      data.frame(type, stringsAsFactors = FALSE) %>%
      group_by(type) %>%
      mutate(num = row_number()) %>%
      ungroup() %>%
      .$num
    
  }else if(num_order == 'text_search'){
    assign_number <- as.numeric(gsub(".*?([0-9]+).*", "\\1", 
                                      assign_names))
  }

  
  #grab the late data
  late <- unlist(lapply(1:num_students, 
                        FUN = function(s) 
                          sapply(1:num_assignments,
                                 FUN = function(a) 
                                   js$students[[s]]$grades[[a]]$late)))
  #grab the image data
  #for each student
    # for each assignment
      # for each post
        # grab the number of images, and store in a list
    #rbind together
  allimages <- do.call('rbind', 
                       lapply(1:num_students, 
                              function(s) 
                                do.call('rbind', 
                                        lapply(
                                          sapply(1:num_assignments, 
                                                 function(a) 
                                                   sapply(js$students[[s]]$grades[[a]]$posts, 
                                                          function(p) p$images)), 
                                          function(i) c('allimages' = list(i))))))
  #grab the post data
  #for each student
    # for each assignment
      # for each post
        # grab the post's length, and store in a list
  #rbind together
  allposts <- do.call('rbind', 
                      lapply(1:num_students, 
                             function(s) 
                               do.call('rbind', 
                                       lapply(
                                         sapply(1:num_assignments, 
                                                function(a) 
                                                  sapply(js$students[[s]]$grades[[a]]$posts, 
                                                         function(p) p$length)), 
                                         function(l) c('allposts' = list(l))))))
  #grab the score data
  # for each student
    # for each assignment
      # grab the score, replacing NULLs with NA
  score <- unlist(lapply(1:num_students, 
                         FUN = function(s) 
                           sapply(1:num_assignments,
                                  FUN = function(a) 
                                    replace_NULL(js$students[[s]]$grades[[a]]$score))))
  #ugly, but avoids nasty for loops
  # for each student
    # for each assignment
      # calculate some summary statistics on post lengths and images
  if(length(js$assignments[[1]]) == 1){
    post_table <- do.call('rbind',
                          lapply(1:num_students,
                                 function(s)
                                   do.call('rbind',
                                           lapply(
                                             sapply(1:num_assignments,
                                                    function(a)
                                                      sapply(js$students[[s]]$grades[[a]]$posts,
                                                             function(p) p$length)),
                                             function(l) c(summary_fun(l, 'post'))))))
    
    image_table <- do.call('rbind',
                           lapply(1:num_students,
                                  function(s)
                                    do.call('rbind',
                                            lapply(
                                              sapply(1:num_assignments,
                                                     function(a)
                                                       sapply(js$students[[s]]$grades[[a]]$posts,
                                                              function(p) p$images)),
                                              function(l) c(summary_fun(l, 'image'))))))
  }else{
    post_table <- do.call('rbind', 
                          apply(allposts, 1, 
                                function(p) summary_fun(unlist(p), 'post')))
    
    image_table <- do.call('rbind', 
                           apply(allimages, 1, 
                                 function(i) summary_fun(unlist(i), 'image')))
  }
  # put all of our collected data together
  dat <- cbind.data.frame(
    'names' = rep(student_names, each = num_assignments),
    'id' = rep(student_id, each = num_assignments),
    'assignment' = rep(assign_number, num_students),
    type = rep(type, num_students),
    assign_name = rep(assign_names, num_students),
    score,
    late,
    post_table,
    allposts,
    image_table,
    allimages,
    assign_prompts,
    stringsAsFactors = FALSE
  )
  #add the post median to the data set
  #can be used as a proxy for prompted number of postings
  dat %>%
    group_by(assignment, type) %>%
    mutate(NumPostMedian = median(post_count)) %>%
    ungroup() %>%
    mutate(post_count = ifelse(is.finite(post_count), post_count, 0),
           post_count_bin = factor(
             ifelse(post_count > 5, '>5', post_count),
             levels = c(-2:5, '>5'),
             ordered = TRUE)) %>%
    as.data.frame
}

########################
### Begin Shiny App ###
#######################


#design the app


function(input, output){
  ##############
  ##Data Steps##
  ##############
  
  #choose the file
  file_to_read <- reactive({
    switch(input$file_selection,
           'Simple (3x3)' = simp_three_by_three,
           'Simple (20x10)' = simp_twenty_by_ten,
           'Real (50x15)' = real_fifty_by_fifteen,
           'Real (100x15)' = real_oneH_by_fifteen,
           'Real (CS 838)' = real_838,
           'Upload my Own' = NULL)
  })
  #user file upload  
  inFile <- reactive({input$json_up})
  #reactive element for a file upload / download from GH
  json_data <- reactive({
    if(input$file_selection == 'Upload my Own'){
      if(is.null(inFile())){
        return(NULL)
      }else{
      read_json(inFile()$datapath, simplifyVector = FALSE)
      }
    }else{
      read_json(file_to_read(), simplifyVector = FALSE)
    }
  })
  #read and process the JSON data
  base_data <- reactive({
    get_post_data(json_data(), num_order = input$assign_num_method)
    })
  #used for subsetting
  late_vector <- reactive({
    if(input$exclude_late){
      c('Not Late')
    }else{
      c('Late', 'Not Late')
    }
  })
  #minimal manipulation
  full_data_prep <- reactive({
    base_data() %>%
    mutate(LateIndicator = ifelse(late > input$late_cut, 'Late', 'Not Late')) %>%
    filter(LateIndicator %in% late_vector()) %>%
    filter(post_count >= (0 + input$exclude_zero))
    }
  )
  #add score filter to display
  output$score_filter <- renderUI({sliderInput('score_filter',
                                              'Select Score Filter',
                                              min = min(full_data_prep()$score,
                                                        na.rm = TRUE),
                                              max = max(full_data_prep()$score,
                                                        na.rm = TRUE),
                                              value = range(full_data_prep()$score,
                                                            na.rm = TRUE))
  })
  #filter appropriately
  full_data <- reactive({
    full_data_prep() %>%
      filter(score >= input$score_filter[1] & score <= input$score_filter[2])
  })
  #data table showing how we've coded the assignments
  output$assign_table <- renderDataTable({
    full_data()[,
                c("names", "id", "assignment", "type", "assign_name", "score", 
                  "late", "allposts", "post_max", "allimages")]
  })
  #show the user the number of observations
  output$json_sample <- renderText(
    tryCatch(paste0('Rows: ', nrow(full_data())),
             error = function(e) 'Please select or upload your data')
  )
  
  ##############
  ## Heat map ##
  ##############
  #allow user to color text in heatmap by the late indicator
  add_text <- reactive({
    if(input$show_values){ #if user wants to see values
          geom_text(aes_string(label = input$heat_color))
    }else{
        NULL
    }
  })
  
  add_tile <- reactive({
    if(input$exclude_late | !input$color_box_late){
      geom_tile()
    }else if (input$color_box_late){
      geom_tile(aes_string(colour = 'LateIndicator'), size = 1.01)
    }
  })
  #nice text coloring to allow for pop out
  color_box_late <- reactive({
    if(input$exclude_late | !input$color_box_late){
      NULL
    }else if(input$color_box_late){
      scale_colour_manual(values = c('Late' = 'blue', 'Not Late' = NA))
    }
  })
  
  legend_box_late <- reactive({
    if(input$exclude_late | !input$color_box_late){
      NULL
    }else if(input$color_box_late){
      guides(colour = guide_legend(override.aes = list(fill = 'white')))
    }
  })
  
  #expression to control display of x-axis labels
  show_x_axis <- reactive({
    if(input$heat_x_show){
      NULL
    }else{
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    }
  })
  # filter heatmap data
  output$hm_type_filter <- renderUI({selectInput('hm_type_filter',
                                              'Filter Assignment Type for Heatmap',
                                              choices = unique(full_data()$type),
                                              selected = unique(full_data()$type),
                                              multiple = TRUE)
  })
  #expression for generation of heatmap data
  heatmap_data <- reactive({
    #choose appropriate assignment types
    dat <- full_data() %>% filter(type %in% input$hm_type_filter)
    #only keep names falling between selected percentiles
    by(dat, dat$names, function(d) mean(d[,input$heat_color])) %>%
      stack(., stringsAsFactors = FALSE) %>%
      mutate(perc_rank = rank(values, ties.method = 'average') / length(values)) %>%
      filter(perc_rank >= input$hm_perc_filter[1] &
             perc_rank <= input$hm_perc_filter[2]) %>%
      select(ind) %>% c %>% unlist %>% as.character -> names_to_keep
    #filter the data
    dat_filter <- dat %>% filter(names %in% names_to_keep)
    #creating an factor to order the names along the vertical axis
    #user controls which value to be ordered by, and to use avg or median
    if(input$heat_fun == 'avg'){
    dat_filter %>%
      mutate(names = reorder(names, 
                             get(input$heat_order,
                                 envir = as.environment(dat_filter)),
                             FUN = mean),
             assignment = factor(assignment)) -> dat_filter
    }else if (input$heat_fun == 'med'){
      dat_filter %>%
        mutate(names = reorder(names, 
                               get(input$heat_order,
                                   envir = as.environment(dat_filter)),
                               FUN = median),
               assignment = factor(assignment)) -> dat_filter
    }else if (input$heat_fun == 'var'){
      dat_filter %>%
        mutate(names = reorder(names, 
                               get(input$heat_order,
                                   envir = as.environment(dat_filter)),
                               FUN = var),
               assignment = factor(assignment)) -> dat_filter
    }
    
    # still working on implementing outlier detection
    #problems:
    #blindly fitting a model is not advised 
    #need to think pretty hard about the coloring
    #should only add one color aesthetic
    # mod1 <- lmer(as.formula(paste0(input$heat_color, ' ~ ',
    #                                    'type * factor(assignment) + (1 | id)')),
    #                  data = dat_filter,
    #                  na.action = na.exclude)
    # 
    # dat_filter$outlier_check <- abs(residuals(mod1) / sd(residuals(mod1))) > 2
    
    dat_filter #return the data
  })

  ##ADD OUTLIER DETECTION?
  ##tbd
  #outlier detection
  #fit random effects model using heatmapped variable
  #random effect for id
  #main effects and two-way interaction for assignment number and type
  
  #####################
  # e.g.
  # mod <- lmer(y ~ x1 * x2 + (1 | id))
  # possible_outlier <- abs(residuals(mod, type = 'normalized')) > input$out_thresh
  #############
  
  # mixed_model <- reactive({
  #   lmer(as.formula(paste0(input$heat_color, ' ~ ',
  #                          'type * factor(assignment) + (1 | id)')),
  #        data = full_data(),
  #        na.action = na.exclude)
  # })
  
  #create the heatmap
  output$heatmap  <- renderPlot({
    
    ggplot(heatmap_data()) +
    aes(x = assignment, 
        y = names)+
    ylab('')+
    aes_string(fill = input$heat_color) +
    #geom_tile() +
    add_tile()+
    add_text()+
    color_box_late()+
    legend_box_late()+
    facet_wrap(~type, scales = c('fixed', 'free')[input$free_x_heat + 1])+
    scale_fill_gradientn(colors = brewer.pal(6, 'YlOrRd'))+
    show_x_axis()+
    theme(axis.text.y = element_text(size = 7))
  })
  #display the data for a selected cell in the heatmap
  output$heatmap_info <- renderPrint({
    nearPoints(
      heatmap_data(),
      input$plot_click, 
      threshold = 100, 
      maxpoints = 1,
      addDist = FALSE
    )
  
  })
  #############
  # Histogram #
  #############
  
  #render display for assignments to filter
  output$hist_filter <- renderUI({selectInput('hist_assign_filter',
                                            'Filter Assignments for Histogram Display
                                            (ctrl + click to select multiple)',
                                            choices = unique(full_data()$assign_name),
                                            selected = unique(full_data()$assign_name),
                                            multiple = TRUE, selectize = FALSE)
  })
  
  ##add option to display vertical lines for mean and median??
  ##TBD
  
  # expression to control facetting on plot
  #should we facet by late indicator and / or assignment type
  #free scales for facets?
  hist_facet <- reactive({
    if(input$exclude_late){
      if(input$facet_hist2){
        facet_wrap(~type, 
                   scales = c('fixed', 'free')[input$free_scales2 + 1]
        )
      }else{
        NULL
      }
    }else{
      if(is.null(input$facet_hist)){
        NULL
      }else if(all(input$facet_hist == 'Late Indicator')){
        facet_wrap(~LateIndicator, scales = c('fixed', 'free')[input$free_scales + 1])
      }else if(all(input$facet_hist == 'Assignment Type')){
        facet_wrap(~type, 
                   scales = c('fixed', 'free')[input$free_scales + 1])
      }else if(all(input$facet_hist == c('Assignment Type','Late Indicator'))){
        facet_grid(LateIndicator ~ type, 
                   scales = c('fixed', 'free')[input$free_scales + 1])
      }else{
        NULL
      }
    }
    
  })
  
  #add vertical line to data
  vertical_line_data <- reactive({
    
    if(input$exclude_late){
      if(input$facet_hist2){
        #need to calculate mean and median by type
        full_data() %>% 
          filter(assign_name %in% input$hist_assign_filter) %>%
          group_by(type) %>%
          summarise_(avg_value = paste('mean(', input$hist_value, ', na.rm = T)'),
                     .dots = as.formula(paste0("~ median(", input$hist_value,
                                               ", na.rm = T)"))) %>%
          setNames(., c('type', 'mean','median'))
      }else{
        full_data() %>% 
          filter(assign_name %in% input$hist_assign_filter) %>%
          summarise_(avg_value = paste('mean(', input$hist_value, ', na.rm = T)'),
                     .dots = as.formula(paste0("~ median(", input$hist_value,
                                               ", na.rm = T)"))) %>%
          setNames(., c('mean','median'))
      }
    }else{
      if(is.null(input$facet_hist)){
        full_data() %>% 
          filter(assign_name %in% input$hist_assign_filter) %>%
          summarise_(avg_value = paste('mean(', input$hist_value, ', na.rm = T)'),
                     .dots = as.formula(paste0("~ median(", input$hist_value,
                                               ", na.rm = T)"))) %>%
          setNames(., c('mean','median'))
      }else if(all(input$facet_hist == 'Late Indicator')){
        #calculate mean and median by late indicator
        full_data() %>% 
          filter(assign_name %in% input$hist_assign_filter) %>%
          group_by(LateIndicator) %>%
          summarise_(avg_value = paste('mean(', input$hist_value, ', na.rm = T)'),
                     .dots = as.formula(paste0("~ median(", input$hist_value,
                                               ", na.rm = T)"))) %>%
          setNames(., c('LateIndicator', 'mean','median'))
      }else if(all(input$facet_hist == 'Assignment Type')){
        #calculate mean and median by type
        full_data() %>% 
          filter(assign_name %in% input$hist_assign_filter) %>%
          group_by(type) %>%
          summarise_(avg_value = paste('mean(', input$hist_value, ', na.rm = T)'),
                     .dots = as.formula(paste0("~ median(", input$hist_value,
                                               ", na.rm = T)"))) %>%
          setNames(., c('type', 'mean','median'))
      }else if(all(input$facet_hist == c('Assignment Type','Late Indicator'))){
        full_data() %>% 
          filter(assign_name %in% input$hist_assign_filter) %>%
          group_by(type, LateIndicator) %>%
          summarise_(avg_value = paste('mean(', input$hist_value, ', na.rm = T)'),
                     .dots = as.formula(paste0("~ median(", input$hist_value,
                                               ", na.rm = T)"))) %>%
          setNames(., c('type','LateIndicator', 'mean','median'))
      }else{
        full_data() %>% 
          filter(assign_name %in% input$hist_assign_filter) %>%
          summarise_(avg_value = paste('mean(', input$hist_value, ', na.rm = T)'),
                     .dots = as.formula(paste0("~ median(", input$hist_value,
                                               ", na.rm = T)"))) %>%
          setNames(., c('mean','median'))
      }
    }
    
    
  })
  

  add_median_line <- reactive({
    if(is.null(input$hist_vert_lines) | !any(input$hist_vert_lines == 'Median')){
      NULL
    }else{
      geom_vline(data = vertical_line_data(),
                 aes(xintercept = median),
                 colour = 'blue', linetype = 'dashed')
    }
  })
  
  add_mean_line <- reactive({
    if(is.null(input$hist_vert_lines) | !any(input$hist_vert_lines == 'Mean')){
      NULL
    }else{
      geom_vline(data = vertical_line_data(),
                 aes(xintercept = mean),
                 colour = 'red', linetype = 'dashed')
    }
  })
  
  
  #create histogram
  output$histogram <- renderPlot({
    ggplot(data = full_data() %>% 
             filter(assign_name %in% input$hist_assign_filter)) +
      aes_string(x = input$hist_value)+
      auto_scale(scale = input$hist_trans)+
      geom_histogram(bins = input$hist_bin)+
      add_median_line()+
      add_mean_line()+
      hist_facet()
  })
  
  
  
  ##############
  #Scatter plot#
  ##############
  #how should we facet the scatter plot
  facet_scatter <- reactive({
    if(input$facet_sc){
      facet_wrap(~type,
                 scales = c('fixed','free')[input$free_sc_scales + 1])
    }else{
      NULL
    }
  })
  #which type of scatter plot?
  scatter_type <- reactive({
    switch(input$sc_type,
           'Jittered' = geom_jitter(alpha = input$sc_opacity),
           'Points' = geom_point(alpha = input$sc_opacity),
           'Bubble' = geom_count(alpha = input$sc_opacity))
  })
  #expression checking for the existence of non-NA assignment prompts
  check_full_data_prompt <- reactive({
    all(is.na(full_data()$assign_prompt))
  })
  #add color radio buttons
  output$scatter_color <- renderUI({
    if (input$exclude_late){
      if(check_full_data_prompt()){
        selectInput('scatter_color',
                     'Color for Scatter Plot',
                     choices = c('None' = 'None',
                                 'Assignment #' = 'assignment',
                                 'Score' = 'score',
                                 'Longest Post' = 'post_max',
                                 'Shortest Post' = 'post_min',
                                 'Average Post Length' = 'post_mean',
                                 'Sum Post Length' = 'post_sum',
                                 'Median Post Length' = 'post_median',
                                 'Number of Posts' = 'post_count',
                                 'Number of Posts (bin)' = 'post_count_bin',
                                 'Number of Images' = 'image_sum',
                                 'Median Number of Posts' = 'NumPostMedian'
                     ), selected = 'None')   
      }else{
        selectInput('scatter_color',
                     'Color for Scatter Plot',
                     choices = c('None' = 'None',
                                 'Assignment #' = 'assignment',
                                 'Score' = 'score',
                                 'Longest Post' = 'post_max',
                                 'Shortest Post' = 'post_min',
                                 'Average Post Length' = 'post_mean',
                                 'Sum Post Length' = 'post_sum',
                                 'Median Post Length' = 'post_median',
                                 'Number of Posts' = 'post_count',
                                 'Number of Posts (bin)' = 'post_count_bin',
                                 'Number of Images' = 'image_sum',
                                 'Median Number of Posts' = 'NumPostMedian',
                                 'Prompt' = 'assign_prompts'
                     ), selected = 'None')
      }
    }else{
      if(check_full_data_prompt()){
        selectInput('scatter_color',
                     'Color for Scatter Plot',
                     choices = c('None' = 'None',
                                 'Assignment #' = 'assignment',
                                 'Score' = 'score',
                                 'Longest Post' = 'post_max',
                                 'Shortest Post' = 'post_min',
                                 'Average Post Length' = 'post_mean',
                                 'Sum Post Length' = 'post_sum',
                                 'Median Post Length' = 'post_median',
                                 'Number of Posts' = 'post_count',
                                 'Number of Posts (bin)' = 'post_count_bin',
                                 'Number of Images' = 'image_sum',
                                 'Median Number of Posts' = 'NumPostMedian',
                                 'Late Indicator' = 'LateIndicator',
                                 'Late' = 'late'
                     ), selected = 'None')
      }else{
        selectInput('scatter_color',
                     'Color for Scatter Plot',
                     choices = c('None' = 'None',
                                 'Assignment #' = 'assignment',
                                 'Score' = 'score',
                                 'Longest Post' = 'post_max',
                                 'Shortest Post' = 'post_min',
                                 'Average Post Length' = 'post_mean',
                                 'Sum Post Length' = 'post_sum',
                                 'Median Post Length' = 'post_median',
                                 'Number of Posts' = 'post_count',
                                 'Number of Posts (bin)' = 'post_count_bin',
                                 'Number of Images' = 'image_sum',
                                 'Median Number of Posts' = 'NumPostMedian',
                                 'Late Indicator' = 'LateIndicator',
                                 'Late' = 'late',
                                 'Prompt' = 'assign_prompts'
                     ), selected = 'None')
      }
    }
  })
  
  #color points
  color_scatter <- reactive({
    if(input$scatter_color == 'None'){
      NULL
    }else{
      aes_string(colour = input$scatter_color)
    }
  })
  
  add_brewer_pallette <- reactive({
    if(input$scatter_color == 'post_count_bin'){
      scale_color_manual(values = brewer.pal(9, 'Blues')[c(-1, -2)])
    }else{
      NULL
    }
  })
  
  #draw a smoothed line?
  scatter_smoothed <- reactive({
    if(input$smooth_sc){
      geom_smooth(se = input$ci_sc_smooth)
    }else{
      NULL
    }
  })
  # select input for assignment type
  output$scat_type_filter <- renderUI({selectInput('scat_type_filter',
                                                 'Filter Assignment Type for Plot',
                                                 choices = unique(full_data()$type),
                                                 selected = unique(full_data()$type),
                                                 multiple = TRUE)
  })
  # filter the scatter data appropriately
  scatter_data <- reactive({
    full_data() %>%
      filter(type %in% input$scat_type_filter)
  })
  
  #make the scatter plot
  output$scatter <- renderPlot({
    
    ggplot(scatter_data()) +
      aes_string(x = input$x_sc,
                 y = input$y_sc)+
      facet_scatter()+
      scatter_type()+
      scatter_smoothed()+
      auto_scale(axis = 'x', scale = input$x_sc_trans)+
      auto_scale(axis = 'y', scale = input$y_sc_trans)+
      color_scatter()+
      add_brewer_pallette()
    
  })
  
  #add reactive expression for brushed data
  #need to transform values appropriately
  #borrowed from https://github.com/rstudio/shiny/issues/1433
  
  brushed_scatter_data <- reactive({
    req(input$sc_plot_brush) #only react to a brushing
    #which variables
    x.axis.var <- input$x_sc
    y.axis.var <- input$y_sc
    #may need to transform in case we use different scales
    #for some reason (?) plot changes for sqrt, but not log2
    x.axis.trans <- switch(input$x_sc_trans,
                           'log2' = identity,
                           'sqrt' = sqrt,
                           'none' = identity)
    y.axis.trans <- switch(input$y_sc_trans,
                           'log2' = identity,
                           'sqrt' = sqrt,
                           'none' = identity)
    #grab the data
    the_data <- scatter_data()
    #filter if we're using a facet
    if(input$facet_sc){
      facet.var <- input$sc_plot_brush$mapping$panelvar1 
      facet.value <- input$sc_plot_brush$panelvar1
      the_data <- filter_(the_data, sprintf("%s == '%s'", facet.var, facet.value))
    }
    #filter the data for brushing
      the_data[x.axis.trans(the_data[[x.axis.var]]) < input$sc_plot_brush$xmax &
               x.axis.trans(the_data[[x.axis.var]]) > input$sc_plot_brush$xmin &
               y.axis.trans(the_data[[y.axis.var]]) < input$sc_plot_brush$ymax &
               y.axis.trans(the_data[[y.axis.var]]) > input$sc_plot_brush$ymin,
               ]
  })
  #show the brushed data
  output$sc_brush_data <- renderPrint({
    brushed_scatter_data()
  })
  
}