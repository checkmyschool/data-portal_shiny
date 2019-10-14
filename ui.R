shinyUI(navbarPage(h5("CheckMySchool Data Portal", style = "color: #ffffff;"), theme = "styles.css",
                   
                   tabPanel(h5("Welcome", style = "color: #ffffff;"),
                            
                            mainPanel(width = 11,
                                      
                                      column(10, offset = 1,
                                             
                                             h1("CheckMySchool School Neediness Index", align = "center"),
                                             
                                             h5("This School Neediness Index Map identifies which among 
                                                  the 44,751 public elementary and secondary schools in the 
                                                  country are in need of resources. The index consists of 
                                                  seven variables grouped into three categories: accessibility (remoteness, 
                                                  percentage of students receiving conditional cash transfers);
                                                  amenities (water access, internet access, electricity access); 
                                                  and classroom condition (student-teacher ratio, student-classroom ratio). 
                                                  It made use of DepEd data from the Enhanced Basic Education Information 
                                                  System (E-BEIS), the National School Building Inventory conducted by the 
                                                  Education Facilities Division (EFD), and the Remoteness Index developed 
                                                  by the School Effectiveness Division (SED).", align = "center"),
                                             
                                             h5("Heather Baier and Angela Yost worked on this map and the study on School Neediness Index. 
                                                  They came to the Philippines last May to July 2018 as Summer Fellows of William & Mary’s Global 
                                                  Research Institute.", align = "center"),
                                             
                                             h1(" "),
                                             
                                             hr(),
                                             
                                             h1(" "),
                                             
                                             h4(tags$b("School Neediness Index Variables and Definitions"), align = "center"),
                                             
                                             div(tableOutput("variables_table"), align = "center"),
                                             
                                             div(img(src='all_logos.png', height = 550, width = 1000), align = "center"),
                                             
                                             hr()
                                             
                                      )
                                      
                            )
                            
                   ),
                   
                   #navbarMenu(h5("About", style = "color: #ffffff;"),
                   
                   tabPanel(h5("User Guide", style = "color: #ffffff;"),
                            
                            h3(strong("School Neediness Index Map"), align = "center"),
                            
                            h5("The first tab of the CMS Data Portal houses a map of every public schools in the Philippines with 
                                                 filters available to choose which schools you would like to display.", align = "center"),
                            
                            h5("Select your desired school pararameters using the selecters and slider inputs on the left to see
                                                 schools on the map that fit your desired specifications.", align = "center"),
                            
                            div(img(src='tutorial1.png', height = 500, width = 900), align = "center"),
                            
                            h1(" "),
                            
                            h3(strong("Data Explorer"), align = "center"),
                            
                            h5("The second tab of the CMS Data Portal, the Data Explorer, works much like the School Neediness Index Map, 
                                                  except the data is displayed in a table instead of a map.", align = "center"),
                            
                            h5("Click on the empty box below a column name to choose 
                                                  the observation you would like to filter for and the table will adjust itself accordingly. For drop down menus, 
                                                  you can filter for multiple variables. Click on the numbered boxes at the bottom right of the page to see the 
                                                  next observations fitting your desired criteria.", align = "center"),
                            
                            div(img(src='tutorial2.png', height = 500, width = 900), align = "center"),
                            
                            h1(" "),
                            
                            h3(strong("School Profiles"), align = "center"),
                            
                            h5("The third tab of the CMS Data Portal, School Profiles, allows you to choose an individual school to see its 
                                                  respective data.", align = "center"),
                            
                            h5("Choose a school region in the right-side panel and continue to filter for School District, Divisions 
                                                 and Name to find you desired school. The table on the top left shows the School Neediness Index data for the chosen school.
                                                 The histogram on the top right shows the distribution of the selected variable. The table on the bottom left shows the 
                                                 basic data for each school. The pie chart on the bottom right shows the gender distribution of the selected school.", align = "center"),
                            
                            div(img(src='tutorial3.png', height = 500, width = 900), align = "center"),
                            
                            h1(" ")
                            
                   ),
                   
                   #tabPanel(h5("Methodology", style = "color: #000000;"),
                   
                   #h2("Methodology heeeerrrreeee")
                   
                   #)
                   
                   #),
                   
                   # tabPanel("About the School Neediness Index",
                   # 
                   #          column(10, offset = 1,
                   # 
                   #          h1("School Neediness Index Methodology", align = "center"),
                   # 
                   #          h1(" "),
                   # 
                   #          h5("The variables in the School Neediness Index were determined based on focus group and
                   #              individual discussions with school teachers and principals in Guimaras and Rizal.", align = "center"),
                   # 
                   #          h1(" "),
                   # 
                   #          hr(),
                   # 
                   #          fluidRow(
                   # 
                   #              column(10, offset = 1,
                   # 
                   #                     tableOutput("variables_table")
                   # 
                   #              )
                   # 
                   #          ),
                   # 
                   #          hr()
                   # 
                   #      )
                   # 
                   # ),
                   
                   tabPanel(h5("School Neediness Index Map", style = "color: #ffffff;"),
                            
                            sidebarLayout(
                              
                              sidebarPanel(
                                
                                selectInput('region_map', "School Region", choices = unique(all_data$region)),
                                
                                selectInput('year_map', "School Year", choices = c(2015, 2016, 2017), selected = 2015),
                                
                                sliderInput("shi_score_map", "School Neediness Index Score", 0, max(all_data$shi_score),
                                            
                                            value = range(0, max(all_data$shi_score)), step = 0.1),
                                
                                sliderInput("stratio_map", "Student Teacher Ratio", min(all_data$student_teacher_ratio), max(all_data$student_teacher_ratio),
                                            
                                            value = range(all_data$student_teacher_ratio), step = 1),
                                
                                sliderInput("scratio_map", "Student Classroom Ratio", min(all_data$student_classroom_ratio), max(all_data$student_classroom_ratio),
                                            
                                            value = range(all_data$student_classroom_ratio), step = 1),
                                
                                selectInput('water_map', "Access to Water", choices = c("Yes", "No"), multiple = TRUE, selected = c("Yes", "No")),
                                
                                selectInput('internet_map', "Access to Internet", choices = c("Yes", "No"), multiple = TRUE, selected = c("Yes", "No")),
                                
                                selectInput('elec_map', "Access to Electricity", choices = c("Yes", "No"), multiple = TRUE, selected = c("Yes", "No")),
                                
                                sliderInput("ri_map", "Remoteness Index", min(all_data$remoteness_index, na.rm = TRUE), max(all_data$remoteness_index, na.rm = TRUE),
                                            
                                            value = range(all_data$remoteness_index, na.rm = TRUE), step = 100),
                                
                                sliderInput("cct_map", "Percentage of Student's Recieving CCT's", min(all_data$cct_percentage, na.rm = TRUE), max(all_data$cct_percentage, na.rm = TRUE),
                                            
                                            value = range(all_data$cct_percentage, na.rm = TRUE), step = 10)
                                
                              ),
                              
                              mainPanel(
                                
                                leafletOutput("map", width = '1150px', height = '850px')
                                
                              )
                              
                            )
                            
                   ),
                   
                   tabPanel(h5("Data Explorer", style = "color: #ffffff;"),
                            
                            DT::dataTableOutput("timeseries_table")
                            
                   ),
                   
                   tabPanel(h5("School Profiles", style = "color: #ffffff;"),
                            
                            sidebarLayout(
                              
                              sidebarPanel(width = 3,
                                           
                                           selectInput('region_profile', "Select Region", choices = c("Select Region" = "",  sort(unique(as.character(all_data$region))))),
                                           
                                           conditionalPanel("input.region_profile",
                                                            
                                                            selectInput('division_profile', "Select Division", choices = c("All Divisions" = "")
                                                                        
                                                            )
                                                            
                                           ),
                                           
                                           conditionalPanel("input.division_profile",
                                                            
                                                            selectInput('district_profile', "Select District", choices = c("All Districts" = "")
                                                                        
                                                            )
                                                            
                                           ),
                                           
                                           
                                           conditionalPanel("input.district_profile",
                                                            
                                                            selectInput('school_profile', "Select School", choices = c("All Schools" = "")
                                                                        
                                                            )
                                                            
                                           ),
                                           
                                           hr(),
                                           
                                           helpText("Select a school to see its resources and classroom conditions and how it stacks up to national averages."),
                                           
                                           leafletOutput('school_select_map')
                                           
                              ),
                              
                              mainPanel(
                                
                                tabsetPanel(
                                  
                                  tabPanel("School Year 2015 - 2016",
                                           
                                           h1(" "),
                                           
                                           fluidRow(
                                             
                                             column(5, style = "background-color: #DCDCDC; border-radius: 5px; height: 500px; height: 700px",
                                                    
                                                    div(h4(tags$u("School Neediness Index Data")), align = "center"),
                                                    
                                                    tableOutput("snitable_profile_2015")
                                                    
                                             ),
                                             
                                             column(1, " "),
                                             
                                             column(5, style = "background-color: #DCDCDC; border-radius: 2px; height: 700px",
                                                    
                                                    div(h4(tags$u("Basic School Data")), align ="center"),#, style="color:red"),
                                                    
                                                    tableOutput("p_table2_2015")
                                                    
                                             ),
                                             
                                             column(1)
                                             
                                           ),
                                           
                                           hr(),
                                           
                                           fluidRow(
                                             
                                             column(5, #style = "border: 2px solid #DCDCDC; border-radius: 2px; height: 500px;",
                                                    
                                                    div(h4(tags$u("Male to Female Student Ratio")), align ="center"),
                                                    
                                                    plotOutput("distPie_2015")
                                                    
                                             ),#columnrowclose
                                             
                                             column(1, h1(" ")),
                                             
                                             column(5, #style = "border: 2px solid #DCDCDC; border-radius: 2px; height: 500px;",
                                                    
                                                    div(h4(tags$u("Distribution of Students with Disabilities")), align ="center"),
                                                    
                                                    highchartOutput("pwdChart_2015")
                                                    
                                             )#columnrowclose
                                             
                                           ),
                                           
                                           hr()#fluidrowclose
                                           
                                  ),#mainpanelrowclose
                                  
                                  tabPanel("School Year 2016 - 2017",
                                           
                                           h1(" "),
                                           
                                           fluidRow(
                                             
                                             column(5, style = "background-color: #DCDCDC; border-radius: 5px; height: 700px",
                                                    
                                                    div(h4(tags$u("School Neediness Index Data")), align = "center"),
                                                    
                                                    tableOutput("snitable_profile_2016")
                                                    
                                             ),
                                             
                                             column(1, " "),
                                             
                                             column(5, style = "background-color: #DCDCDC; border-radius: 2px; height: 700px",
                                                    
                                                    div(h4(tags$u("Basic School Data")), align ="center"),#, style="color:red"),
                                                    
                                                    tableOutput("p_table2_2016")
                                                    
                                             ),
                                             
                                             column(1)
                                             
                                           ),
                                           
                                           hr(),
                                           
                                           fluidRow(
                                             
                                             column(5, #style = "border: 2px solid #DCDCDC; border-radius: 2px; height: 500px;",
                                                    
                                                    div(h4(tags$u("Male to Female Student Ratio")), align ="center"),
                                                    
                                                    plotOutput("distPie_2016")
                                                    
                                             ),#columnrowclose
                                             
                                             column(1, h1(" ")),
                                             
                                             column(5, #style = "border: 2px solid #DCDCDC; border-radius: 2px; height: 500px;",
                                                    
                                                    div(h4(tags$u("Distribution of Students with Disabilities")), align ="center"),
                                                    
                                                    highchartOutput("pwdChart_2016")
                                                    
                                             )#columnrowclose
                                             
                                           ),
                                           
                                           hr()#fluidrowclose
                                           
                                  ),
                                  
                                  tabPanel("School Year 2017 - 2018",
                                           
                                           fluidRow(
                                             
                                             column(5, style = "background-color: #DCDCDC; border-radius: 5px; height: 700px",
                                                    
                                                    div(h4(tags$u("School Neediness Index Data")), align = "center"),
                                                    
                                                    tableOutput("snitable_profile_2017")
                                                    
                                             ),
                                             
                                             column(1, " "),
                                             
                                             column(5, style = "background-color: #DCDCDC; border-radius: 2px; height: 700px",
                                                    
                                                    div(h4(tags$u("Basic School Data")), align ="center"),#, style="color:red"),
                                                    
                                                    tableOutput("p_table2_2017")
                                                    
                                             ),
                                             
                                             column(1)
                                             
                                           ),
                                           
                                           hr(),
                                           
                                           fluidRow(
                                             
                                             column(5, #style = "border: 2px solid #DCDCDC; border-radius: 2px; height: 500px;",
                                                    
                                                    div(h4(tags$u("Male to Female Student Ratio")), align ="center"),
                                                    
                                                    plotOutput("distPie_2017")
                                                    
                                             ),#columnrowclose
                                             
                                             column(1, h1(" ")),
                                             
                                             column(5, #style = "border: 2px solid #DCDCDC; border-radius: 2px; height: 500px;",
                                                    
                                                    div(h4(tags$u("Distribution of Students with Disabilities")), align ="center"),
                                                    
                                                    highchartOutput("pwdChart_2017")
                                                    
                                             )#columnrowclose
                                             
                                           ),
                                           
                                           hr()#fluidrowclose
                                           
                                  )
                                  
                                )#sidebarlayoutclose
                                
                              )
                              
                            )
                            
                   ),
                   
                   tabPanel(h5("Data Set Builder", style = "color: #ffffff;"),
                            
                            fluidRow(
                              
                              column(width = 4, offset = 1, style = "background-color: #DCDCDC; border-radius: 2px;",
                                     
                                     h3("Choose Columns"),
                                     
                                     checkboxGroupInput('columns', label = 'Choose Columns to include in CSV', choices = c('Remoteness Index' = 'remoteness_index', 
                                                                                                                           "Total Number of Learners Receiving CCT's" = 'total_recieving_cct', 
                                                                                                                           "Percentage of Students Recieving CCT's" = 'cct_percentage', 
                                                                                                                           'Water Access' = 'original_water_boolean', 
                                                                                                                           'Internet Access' = 'original_internet_boolean',
                                                                                                                           'Electricty Access' = 'original_electricity_boolean',
                                                                                                                           'Total Number of Learners with Gender Distribution' = 'total_enrollment',
                                                                                                                           "Total Number of Learners With Disability" = 'pwds')),
                                     
                                     conditionalPanel("input.columns.indexOf('Total Number of Learners With Disability') != -1",
                                                      
                                                      checkboxGroupInput('pwd_breakdown', label = NULL, choices = c('Difficulty Seeing Manifestation' = 'ds_total',
                                                                                                                    'Cerebral Palsy' = 'cp_total',
                                                                                                                    "Difficulty Communicating Manifestation" = 'dcm_total',
                                                                                                                    "Difficulty Remembering, Concentrating, Paying Attention and Understanding based on Manifestation" = 'drcpau_total',
                                                                                                                    "Difficulty Hearing Manifestation" = 'dh_total',
                                                                                                                    "Autism Spectral Disorder" = 'autism_total',
                                                                                                                    "Difficulty Walking, Climbing and Grasping" = 'wcg_total',
                                                                                                                    "Emotional-Behavioral Disorder" = 'eb_total',
                                                                                                                    "Hearing Impairment" = 'hi_total',
                                                                                                                    "Intellectual Impairment" = 'id_total',
                                                                                                                    "Learning Impairment" = 'li_total',
                                                                                                                    "Multiple Disabilities" = 'md_total',
                                                                                                                    "Orthopedic/Physical Disorder" = 'pd_total',
                                                                                                                    "Special Health Problem/Chronic Illness" = 'shp_total',
                                                                                                                    "Speech Disorder" = 'speech_total',
                                                                                                                    "Visual Impairment Disorder" = 'vi_total',
                                                                                                                    "Intellectual Impairment" = 'ii_total',
                                                                                                                    "Orthopedic/Physical Disorder" = 'p_total'))
                                                      
                                     )
                                     
                              ),
                              
                              column(width = 2,
                                     
                                     h1(" ")
                                     
                              ),
                              
                              column(width = 4, style = "background-color: #DCDCDC; border-radius: 2px;",
                                     
                                     h3("Choose Rows"),
                                     
                                     radioButtons('FilterGeo', label = 'Choose geographic breakdown to filter data',
                                                  
                                                  choices = c('School Region', 'School Province', 'School Division', 'School District', 'School Municipality')
                                                  
                                     ),
                                     
                                     conditionalPanel("input.FilterGeo == 'School Region'",
                                                      
                                                      selectInput('QueryRegion', "Choose School Regions", choices = unique(all_data$region), multiple = TRUE)
                                                      
                                     ),
                                     
                                     conditionalPanel("input.FilterGeo == 'School Province'",
                                                      
                                                      selectInput('QueryProvince', "Choose School Provinces", choices = unique(all_data$province), multiple = TRUE)
                                                      
                                     ),
                                     
                                     conditionalPanel("input.FilterGeo == 'School Division'",
                                                      
                                                      selectInput('QueryDivision', "Choose School Divisions", choices = unique(all_data$division), multiple = TRUE)
                                                      
                                     ),
                                     
                                     conditionalPanel("input.FilterGeo == 'School District'",
                                                      
                                                      selectInput('QueryDistrict', "Choose School Districts", choices = unique(all_data$district), multiple = TRUE)
                                                      
                                     ),
                                     
                                     conditionalPanel("input.FilterGeo == 'School Municipality'",
                                                      
                                                      selectInput('QueryMunicipality', "Choose School Municipalities", choices = unique(all_data$municipality), multiple = TRUE)
                                                      
                                     ),
                                     
                                     
                                     div(downloadButton('QueryBuilder', h4('Download CSV')), align = 'center')
                                     
                              )
                              
                            ),
                            
                            hr(),
                            
                            div(tags$u(h3("Data Set Preview")), align = 'center'),
                            
                            h1(" "),
                            
                            DT::dataTableOutput("QueryTablePreview")
                            
                   )
                   
)

)