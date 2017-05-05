#load packages to run app
library(shiny)
library(shinythemes)
library(DT)
#set up document
fluidPage(theme = shinytheme('simplex'),
  #title
  h2('CS 765 Spring 2017 Student Data Visualization'),
  h5('Code for this app can be found', 
     a('here.', href = 'https://github.com/bgstieber/CS765StudentVis')),
  #define page 
  #will be three tabs for visualizations
  #one side panel for data stuff
  fluidRow(column(10,
           tabsetPanel(
             # heat map panel
             tabPanel('Assignment Heatmap',
               sidebarLayout(
                 sidebarPanel(
                        h2('Heatmap Parameters'),
                        #what should we show on the heatmap
                        selectInput('heat_color',
                                    'Heatmap Color',
                                    c('Score' = 'score',
                                      'Longest Post' = 'post_max',
                                      'Shortest Post' = 'post_min',
                                      'Average Post Length' = 'post_mean',
                                      'Sum Post Length' = 'post_sum',
                                      'Median Post Length' = 'post_median',
                                      'Number of Posts' = 'post_count',
                                      'Number of Images' = 'image_sum'
                                    )
                                    ),
                        # how should the heatmap be ordered
                        selectInput('heat_order',
                                    'Order Heatmap Tiles By',
                                    c('Score' = 'score',
                                      'Longest Post' = 'post_max',
                                      'Shortest Post' = 'post_min',
                                      'Average Post Length' = 'post_mean',
                                      'Sum Post Length' = 'post_sum',
                                      'Median Post Length' = 'post_median',
                                      'Number of Posts' = 'post_count',
                                      'Number of Images' = 'image_sum'
                                    )
                                    ),
                        #choose aggregation for ordering
                        radioButtons('heat_fun',
                                    'Ordering Aggregate Function',
                                    choices = c('Average' = 'avg',
                                                'Median' = 'med',
                                                'Variance' = 'var')),
                        h5('Students will be ordered according to the average,
                            median, or variance of the selected heatmap order field. 
                            The students will then be displayed in descending order (highest
                           on top, lowest on bottom)'),
                        uiOutput('hm_type_filter'), #assignment type filter
                        #percentile filter
                        sliderInput('hm_perc_filter',
                                    'Filter Percentiles',
                                    min = 0, max = 1,
                                    value = c(0, 1)
                                    ),
                        h4("Percentiles are calculated based on the individual's
                           average value for the measure you are visualizing.
                           Missing values are discarded when the average is
                           calculated."),
                        #choices that affect the display
                        #show x-axis labels, free scales for facetting
                        #show values on heatmap
                        checkboxInput('heat_x_show',
                                      'Show x-axis labels',
                                      TRUE),
                        checkboxInput('free_x_heat',
                                      'Free x-axis scales',
                                      FALSE),
                        checkboxInput('show_values',
                                      'Display Values on Heatmap',
                                      FALSE),
                        #should we color the text by the late indicator
                        conditionalPanel("!input.exclude_late",
                                         checkboxInput('color_box_late',
                                                       'Color Cells by Late Indicator',
                                                       FALSE)
                                         )
                        ),
                 #generate the heatmap
                 mainPanel(
                   plotOutput(outputId = 'heatmap',
                              height = '700px',
                              click = 'plot_click'),
                   verbatimTextOutput('heatmap_info')
                 )
                )
               ),
             # histogram panel
             tabPanel('Histogram',
               sidebarLayout(
                 sidebarPanel(
                        h2('Histogram Parameters'),
                        #what should the histogram visualize
                        selectInput('hist_value',
                                    'Histogram Measure',
                                    c('Score' = 'score',
                                      'Longest Post' = 'post_max',
                                      'Shortest Post' = 'post_min',
                                      'Average Post Length' = 'post_mean',
                                      'Sum Post Length' = 'post_sum',
                                      'Median Post Length' = 'post_median',
                                      'Number of Posts' = 'post_count',
                                      'Number of Images' = 'image_sum'
                                    )),
                        #number of bins for histogram
                        sliderInput('hist_bin', label = 'Histogram Bins',
                                    min = 10, max = 200, value = 30),
                        #transformation for variable we're visualizing
                        radioButtons('hist_trans',
                                     label = 'Variable Transformation',
                                     choices = c('None' = 'none',
                                                 'Square Root' = 'sqrt',
                                                 'log base 2' = 'log2')
                                     ),
                        conditionalPanel(
                          "input.hist_trans == 'log2'",
                          h6('For the transformation, note that if the variable is less 
                             than or equal to 0, some items will be excluded from the
                             visualization.')
                          ),
                        conditionalPanel(
                          "input.hist_trans == 'sqrt'",
                          h6('For the transformation, note that if the variable is less 
                             than 0, some items will be excluded from the
                             visualization.')
                          ),
                        #add vertical lines for median and means
                        checkboxGroupInput('hist_vert_lines',
                                           label = 'Add Vertical Lines For',
                                           choices = c(
                                             'Median (blue)' = 'Median',
                                             'Mean (red)' = 'Mean'
                                           )),
                        #facetting histogram view
                        conditionalPanel("!input.exclude_late",
                                         checkboxGroupInput('facet_hist',
                                                      label = 'Facet Histogram By',
                                                      choices = c(
                                                        'Assignment Type',
                                                        'Late Indicator'
                                                      )
                                                  ),
                                         conditionalPanel(
                                           "input.facet_hist.length > 0",
                                           checkboxInput("free_scales",
                                                         "Free Scales for Facetting")
                                         )
                                         ),
                        conditionalPanel("input.exclude_late",
                                         checkboxInput('facet_hist2',
                                          'Facet Histogram By Assignment Type'
                                         ),
                                         conditionalPanel(
                                           "input.facet_hist2",
                                           checkboxInput(
                                             "free_scales2",
                                             "Free Scales for Facetting"
                                           )
                                         )
                                         )
                        ,uiOutput('hist_filter')
                        ),
                 mainPanel(plotOutput(outputId = 'histogram'),
                           dataTableOutput('hist_table_check'))
                      )
             ),
             # scatter plot panel
             tabPanel('Brushed Scatterplot',
               sidebarLayout(
                 sidebarPanel(
                        h2('Scatterplot Parameters'),
                        selectInput('x_sc','X',
                                    c('Assignment #' = 'assignment',
                                      'Score' = 'score',
                                      'Longest Post' = 'post_max',
                                      'Shortest Post' = 'post_min',
                                      'Average Post Length' = 'post_mean',
                                      'Sum Post Length' = 'post_sum',
                                      'Median Post Length' = 'post_median',
                                      'Number of Posts' = 'post_count',
                                      'Number of Images' = 'image_sum',
                                      'Median Number of Posts' = 'NumPostMedian'
                                    ), selected = 'post_mean'),
                        selectInput('y_sc','Y',
                                    c('Assignment #' = 'assignment',
                                      'Score' = 'score',
                                      'Longest Post' = 'post_max',
                                      'Shortest Post' = 'post_min',
                                      'Average Post Length' = 'post_mean',
                                      'Sum Post Length' = 'post_sum',
                                      'Median Post Length' = 'post_median',
                                      'Number of Posts' = 'post_count',
                                      'Number of Images' = 'image_sum',
                                      'Median Number of Posts' = 'NumPostMedian'
                                    ), selected = 'score'),
                        uiOutput('scatter_color'),
                        selectInput('x_sc_trans',
                                     label = 'X Transformation',
                                     choices = c('None' = 'none',
                                                 'Square Root' = 'sqrt',
                                                 'log base 2' = 'log2')),
                        conditionalPanel(
                          "input.x_sc_trans == 'log2'",
                          h6('For the transformation, note that if the variable is less 
                             than or equal to 0, some items will be excluded from the
                             visualization.')
                          ),
                        conditionalPanel(
                          "input.x_sc_trans == 'sqrt'",
                          h6('For the transformation, note that if the variable is less 
                             than 0, some items will be excluded from the
                             visualization.')
                          ),
                        selectInput('y_sc_trans',
                                     label = 'Y Transformation',
                                     choices = c('None' = 'none',
                                                 'Square Root' = 'sqrt',
                                                 'log base 2' = 'log2')),
                        conditionalPanel(
                          "input.y_sc_trans == 'log2'",
                        h6('For the transformation, note that if the variable is less 
                           than or equal to 0, some items will be excluded from the
                           visualization.')
                        ),
                        conditionalPanel(
                          "input.y_sc_trans == 'sqrt'",
                          h6('For the transformation, note that if the variable is less 
                             than 0, some items will be excluded from the
                             visualization.')
                          ),
                        selectInput('sc_type', 'Plot Type',
                                    c('Points', 'Jittered', 'Bubble'),
                                    'Points'),
                        conditionalPanel(
                          "input.sc_type == 'Jittered'",
                          h6('Jittering adds a small amount of random variation to 
                             the location of each point, and is a useful way of 
                             handling overplotting caused by discreteness 
                             in smaller datasets. Note that the scatter plot
                             brushing may not work as expected when the points
                             are jittered.')
                        ),
                        conditionalPanel(
                          "input.sc_type == 'Bubble'",
                          h6("A bubble plot counts the number of observations at 
                             each location, then maps the count to point area. 
                             It can be useful when you have discrete 
                             data and overplotting.",
                             a("An excellent example of a bubble plot", 
                               href = "http://www.gapminder.org/tools/#_locale_id=en;&chart-type=bubbles"),
                             "can be found on the Gapminder website.")
                        ),
                        uiOutput('scat_type_filter'),
                        sliderInput('sc_opacity',
                                    'Opacity of Points',
                                    min = 0, max = 1,
                                    value = 0.8),
                        checkboxInput('facet_sc',
                                      label = 'Facet Plot By Assignment Type'
                        ),
                        conditionalPanel("input.facet_sc",
                                         checkboxInput(
                                           'free_sc_scales',
                                           'Free Scales for Facetting',
                                           FALSE
                                         )
                                         ),
                        checkboxInput('smooth_sc',
                                      'Add Smoothed Line'),
                        conditionalPanel("input.smooth_sc",
                                         checkboxInput('ci_sc_smooth',
                                                       'Draw 95% CI for Smotthed Line'))
                          
                        ),
                 mainPanel(
                        plotOutput(outputId = 'scatter', brush = 'sc_plot_brush'),
                        verbatimTextOutput('sc_brush_data')
                        )
             )
           ),
           tabPanel('Data Table View',
                    mainPanel(dataTableOutput("assign_table"))
             
           )
        )
    ),
    #sidebar for data options
    column(2,
           h2('Data Options'),
           #file selection
           h4('Choose a File'),
           radioButtons(inputId = 'file_selection',
                        label = NULL,
                        choices = c('Simple (3x3)', 'Simple (20x10)', 
                                    'Real (50x15)', 'Real (100x15)',
                                    'Real (CS 838)',
                                    'Upload my Own')),
           
           conditionalPanel("input.file_selection == 'Upload my Own'",
                            fileInput('json_up',
                                      'Choose JSON file',
                                      accept = c('.json'))
           ),
           radioButtons('assign_num_method',
                        'Method for Determining Assignment Number',
                        c('Data set order' = 'row_num',
                          'Extract number from assignment name' = 'text_search'),
                        'row_num'
           ),
           #text regarding data input
           actionButton('hide_text', 'Hide Text'),
           conditionalPanel("input.hide_text % 2 == 0",
                            conditionalPanel("input.assign_num_method == 'row_num'",
                                             h5(p('The data set order option assumes the 
                                                    assignments (within each assignment type) were entered in the 
                                                    JSON file in chronological order. The 838 data set is an example of
                                                    a file', strong('not'), 'in chronological order.'))
                            ),
                            conditionalPanel("input.assign_num_method == 'text_search'",
                                             h5(p('The extract number from assignment name method
                                                    uses regular expressions to attempt to
                                                    extract the number from the name of the assignment.
                                                    This method can be faulty if two assignments
                                                    share a number (e.g. 24A and 24B).',
                                                  a('Obligatory XKCD strip',
                                                    href = 'https://xkcd.com/1171/')
                                             ))
                            ),
                            h5(p('We have included a tab with a data table that demonstrates
                                 the results of our data cleaning and organizing
                                 process. The table only includes a subset of the
                                 columns in our full data set.'))
                            ),
           #how should late values for handled
           h4('Late Cutoff'),
           numericInput('late_cut', NULL, value = 0),
           h5('For the late cutoff, all assignments later than the cutoff will be marked late.'),
           h4('Exclusions'),
           checkboxInput('exclude_late', 'Exclude Late Assignments', FALSE),
           checkboxInput('exclude_zero', 'Exclude Records with Zero Posts',
                         FALSE),
           uiOutput('score_filter'),
           h3('Number of Observations'),
           verbatimTextOutput('json_sample')
           )
    )
  )