shinyServer(function(input, output, session) {
    
    
    # SNI Methodology Table ---------------------------------------------------
    output$variables_table <- renderTable(methodology, colnames = TRUE, striped = TRUE, hover = TRUE, align = "c")
    
    # Time Series Map ---------------------------------------------------------
    mapdata_react <- reactive({
        map_data <- all_data[all_data$region == input$region_map,]
        print(dim(map_data)[1])
        map_data <- map_data[map_data$school_year == input$year_map,]
        print(dim(map_data)[1])
        
        map_data <- subset(map_data, shi_score >= input$shi_score_map[1] & shi_score <= input$shi_score_map[2])
        
        print(dim(map_data)[1])
        
        # map_data <- filter(map_data, shi_score >= input$shi_score_map[1] & shi_score <= input$shi_score_map[2])
        map_data <- subset(map_data, student_teacher_ratio >= input$stratio_map[1] & student_teacher_ratio <= input$stratio_map[2])
        print(dim(map_data)[1])
        
        map_data <- subset(map_data, student_classroom_ratio >= input$scratio_map[1] & student_classroom_ratio <= input$scratio_map[2])
        print(dim(map_data)[1])
        
        map_data <- map_data[map_data$original_water_boolean %in% input$water_map,]
        print(dim(map_data)[1])
        
        map_data <- map_data[map_data$original_internet_boolean %in% input$internet_map,]
        print(dim(map_data)[1])
        
        map_data <- map_data[map_data$original_electricity_boolean %in% input$elec_map,]
        print(dim(map_data)[1])
        
        map_data <- subset(map_data, remoteness_index >= input$ri_map[1] & remoteness_index <= input$ri_map[2])
        print(dim(map_data)[1])
        
        map_data <- subset(map_data, cct_percentage >= input$cct_map[1] & cct_percentage <= input$cct_map[2])
        print(dim(map_data)[1])
        
        map_data
    })
    
    output$map <- renderLeaflet({
        
        print(dim(mapdata_react())[1])
        
        leaflet(data = mapdata_react()) %>%
            clearMarkerClusters() %>%
            addTiles(
                urlTemplate = 'https://maps.wikimedia.org/osm-intl/{z}/{x}/{y}{r}.png',
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
            setView(lat = 12.8797, lng = 122.7740, zoom = 6) %>%
            addMarkers(
                clusterOptions = markerClusterOptions(), popup = ~paste("<b>School Name:</b>", mapdata_react()$school_name, "<br/>",
                                                                        "<b>School ID:</b>", mapdata_react()$school_id, "<br/>",
                                                                        "<b>School Neediness Score:</b>", mapdata_react()$shi_score, "<br/>",
                                                                        "<b>Student Teacher Ratio:</b>", mapdata_react()$student_teacher_ratio, "<br/>",
                                                                        "<b>Student Classroom Ratio:</b>", mapdata_react()$student_classroom_ratio, "<br/>",
                                                                        "<b>Water Access:</b>", mapdata_react()$original_water_boolean, "<br/>",
                                                                        "<b>Internet Access:</b>", mapdata_react()$original_internet_boolean, "<br/>",
                                                                        "<b>Electricity Access:</b>", mapdata_react()$original_electricity_boolean, "<br/>",
                                                                        "<b>Remoteness Index:</b>", mapdata_react()$remoteness_index, "<br/>",
                                                                        "<b>CCT Percentage:</b>", mapdata_react()$cct_percentage, "<br/>")
            )
    })
    
    
    
    # Time Series Data Table --------------------------------------------------
    output$timeseries_table <- DT::renderDataTable(DT::datatable(data = ts_clean, options = list(autoWidth = FALSE), filter = "top"))
    
    
    # School Profiles ---------------------------------------------------------
    
    observe({
        all_data <- all_data[all_data$region == input$region_profile,]
        updateSelectInput(session, "division_profile", choices = c("All Divisions" = "", sort(unique(as.character(all_data$division)))))
    })
    
    
    observe({
        if (input$division_profile != "") {
            all_data <- all_data[all_data$region == input$region_profile,]
            all_data <- all_data[all_data$division == input$division_profile,]
            updateSelectInput(session, "district_profile", choices = c("All Districts" = "",  sort(unique(as.character(all_data$district)))))
        }
    })
    
    observe({
        if (input$district_profile != "") {
            all_data <- all_data[all_data$region == input$region_profile,]
            all_data <- all_data[all_data$division == input$division_profile,]
            all_data <- all_data[all_data$district == input$district_profile,]
            updateSelectInput(session, "school_profile", choices = c("All Schools" = "",  sort(unique(as.character(all_data$school_name)))))
        }
    })
    
    
    
    
    # School Year 2015 - 2016 -------------------------------------------------
    sy_1516_data_react <- reactive({
        sy1516_data <- all_data[all_data$school_year == 2015,]
        sy1516_data <- sy1516_data[sy1516_data$region == input$region_profile,]
        sy1516_data <- sy1516_data[sy1516_data$division == input$division_profile,]
        sy1516_data <- sy1516_data[sy1516_data$district == input$district_profile,]
        sy1516_data <- sy1516_data[sy1516_data$school_name == input$school_profile,]
        print(colnames(sy1516_data))
        return(sy1516_data)
    })
    
    
    output$school_select_map <- renderLeaflet({
        
        leaflet() %>%
            clearMarkerClusters() %>%
            addTiles() %>%
            addCircleMarkers(data = sy_1516_data_react())
    })
    
    profile_2015_data_react <- reactive({
        profile_2015_vars <- c('school_name', "school_id", "region",
                               "division", "district", 'shi_score', 'sni_percentile_text',
                               'remoteness_index', 'remoteness_cluster', 'remoteness_percentile_cluster', 'cct_percentage',
                               'student_teacher_ratio', 'student_classroom_ratio',
                               'original_water_boolean', 'original_internet_boolean',
                               'original_electricity_boolean')
        profile_2015_data <- sy_1516_data_react()[profile_2015_vars]
        
        colnames(profile_2015_data) <- c(
            'School Name',
            "School ID",
            "Region",
            "Division",
            "District",
            'SNI Score',
            'SNI Percentile',
            'Remoteness Index',
            'Remoteness Cluster',
            'Remoteness Percentile',
            'CCT Percentage',
            'Student Teacher Ratio',
            'Student Classroom Ratio',
            'Water Access',
            'Internet Access',
            'Electricity Access'
        )
        
        profile_2015_data <- as.data.frame(t(profile_2015_data))
        profile_2015_data <- tibble::rownames_to_column(profile_2015_data, " ")
        colnames(profile_2015_data) <- c(" ", " ")
        return(profile_2015_data)
    })
    
    output$snitable_profile_2015 <- renderTable({
        shiny::validate(need(input$school_profile != "", "Please choose a school for details."))
        shiny::validate(need(dim(sy_1516_data_react())[1] != 0, "No data available for selected school in school year 2015 - 2016"))
        profile_2015_data_react()
        
    })
    
    
    pie_react_2015 <- reactive({
        pie_2015_data <- c(sy_1516_data_react()$total_female, sy_1516_data_react()$total_male)
        print(pie_2015_data)
        return(pie_2015_data)
    })
    
    output$distPie_2015 <- renderPlot({
        shiny::validate(need(input$school_profile != "", "Please choose a school for details."))
        shiny::validate(need(dim(sy_1516_data_react())[1] != 0, "No data available for selected school in school year 2015 - 2016"))
        pie(pie_react_2015(), labels = c("Female", "Male"), col = c('#e6a6c7', '#347dc1'))#, radius = 4)#, main, col, clockwise)
    })
    
    
    basic_data_react_2015 <- reactive({
        basic_2015_vars <- c("school_year", "school_name", "province", 
                             "municipality", "region", "district", 
                             "division", "total_enrollment", 
                             "total_female", "total_male")
        basic_2015_data <- sy_1516_data_react()[basic_2015_vars]
        print(head(basic_2015_data))
        
        colnames(basic_2015_data) <- c('School Year', "School Name", "Province",
                                       "Municipality", "Region", "District", 
                                       "Division", "Total Enrollment", 
                                       "Total Female", "Total Male")
        
        basic_2015_data <- as.data.frame(t(basic_2015_data))
        basic_2015_data <- tibble::rownames_to_column(basic_2015_data, " ")
        
        print("basic data table:")
        
        print(basic_2015_data)
        
        colnames(basic_2015_data) <- c(" ", " ")
        return(basic_2015_data)
    })
    
    output$p_table2_2015 <- renderTable({
        shiny::validate(need(input$school_profile != "", "Please choose a school for details."))
        shiny::validate(need(dim(sy_1516_data_react())[1] != 0, "No data available for selected school in school year 2015 - 2016"))
        basic_data_react_2015()
    })
    
    
    output$pwdChart_2015 <- renderHighchart({
        
        shiny::validate(need(input$school_profile != "", "Please choose a school for details."))
        shiny::validate(need(dim(sy_1516_data_react())[1] != 0, "No data available for selected school in school year 2015 - 2016"))
        shiny::validate(need(sy_1516_data_react()$pwd_total != 0, "No students with disabilites at selected school"))
        
        print(sy_1516_data_react())
        pwd_2015_vars <- c("ds_total",
                           "cp_total", "dcm_total", "drcpau_total", "dh_total",
                           "autism_total", "wcg_total", "eb_total", "hi_total",
                           "id_total", "li_total", "md_total", "pd_total",
                           "shp_total", "speech_total", "vi_total", "ii_total",                           
                           "p_total")
        
        pwd_2015_data <- sy_1516_data_react()[pwd_2015_vars]
        colnames(pwd_2015_data) <- c("Difficulty Seeing Manifestation",
                                     "Cerebral Palsy",
                                     "Difficulty Communicating Manifestation",
                                     "Difficulty Remembering, Concentrating, Paying Attention and Understanding based on Manifestation",
                                     "Difficulty Hearing Manifestation",
                                     "Autism Spectral Disorder",
                                     "Difficulty Walking, Climbing and Grasping",
                                     "Emotional-Behavioral Disorder",
                                     "Hearing Impairment",
                                     "Intellectual Impairment",
                                     "Learning Impairment",
                                     "Multiple Disabilities",
                                     "Orthopedic/Physical Disorder",
                                     "Special Health Problem/Chronic Illness",
                                     "Speech Disorder",
                                     "Visual Impairment Disorder",
                                     "Intellectual Impairment",
                                     "Orthopedic/Physical Disorder")
        pwd_2015_data <- as.data.frame(t(pwd_2015_data))
        pwd_2015_data <- tibble::rownames_to_column(pwd_2015_data, "Disability")
        colnames(pwd_2015_data) <- c("Disability", "Value")
        pwd_2015_data <- pwd_2015_data[pwd_2015_data$Value > 0,]
        hc <- pwd_2015_data %>%
            hchart(type = "column", hcaes(x = Disability, y = Value)) %>%
            hc_xAxis(title = list(text = "Disability")) %>%
            hc_yAxis(title = list(text = "Value"))
        return(hc)
        
    })
    
    
    
    
    
    
    
    
    
    # School Profiles 2016 - 2017 ---------------------------------------------
    sy_1617_data_react <- reactive({
        sy1617 <- all_data[all_data$school_year == 2016,]
        sy1617 <- sy1617[sy1617$region == input$region_profile,]
        sy1617 <- sy1617[sy1617$division == input$division_profile,]
        sy1617 <- sy1617[sy1617$district == input$district_profile,]
        sy1617 <- sy1617[sy1617$school_name == input$school_profile,]
        colnames(sy1617)
    })
    
    
    profile_2016_data_react <- reactive({
        
        profile_2016_vars <- c('school_name', "school_id", "region",
                               "division", "district", 'shi_score', 'sni_percentile_text',
                               'remoteness_index', 'remoteness_cluster', 'remoteness_percentile_text', 'cct_percentage',
                               'student_teacher_ratio', 'student_classroom_ratio',
                               'original_water_boolean', 'original_internet_boolean',
                               'original_electricity_boolean')
        
        profile_2016_data <- sy_1617_data_react()[profile_2016_vars]
        
        colnames(profile_2016_data) <- c(
            'School Name',
            "School ID",
            "Region",
            "Division",
            "District",
            'SNI Score',
            'SNI Percentile',
            'Remoteness Index',
            'Remoteness Cluster',
            'Remoteness Percentile',
            'CCT Percentage',
            'Student Teacher Ratio',
            'Student Classroom Ratio',
            'Water Access',
            'Internet Access',
            'Electricity Access'
        )
        
        profile_2016_data <- as.data.frame(t(profile_2016_data))
        profile_2016_data <- tibble::rownames_to_column(profile_2016_data, " ")
        colnames(profile_2016_data) <- c(" ", " ")
        return(profile_2016_data)
    })
    
    output$snitable_profile_2016 <- renderTable({
        shiny::validate(need(input$school_profile != "", "Please choose a school for details."))
        shiny::validate(need(dim(sy_1617_data_react())[1] != 0, "No data available for selected school in school year 2016 - 2017"))
        profile_2016_data_react()
        
    })
    
    
    pie_react_2016 <- reactive({
        pie_2016_data <- c(sy_1617_data_react()$total_female, sy_1617_data_react()$total_male)
        print(pie_2016_data)
        return(pie_2016_data)
    })
    
    output$distPie_2016 <- renderPlot({
        shiny::validate(need(input$school_profile != "", "Please choose a school for details."))
        shiny::validate(need(dim(sy_1617_data_react())[1] != 0, "No data available for selected school in school year 2016 - 2017"))
        pie(pie_react_2016(), labels = c("Female", "Male"), col = c('#e6a6c7', '#347dc1'))#, radius = 4)#, main, col, clockwise)
    })
    
    
    basic_2016_data <- reactive({
        basic_2016_vars <- c("school_year", "school_name", "province", 
                             "municipality", "region", "district", 
                             "division", "total_enrollment", 
                             "total_female", "total_male")
        basic_2016_data <- sy_1617_data_react()[basic_2016_vars]
        print(head(basic_2016_data))
        
        colnames(basic_2016_data) <- c('School Year', "School Name", "Province",
                                       "Municipality", "Region", "District", 
                                       "Division", "Total Enrollment", 
                                       "Total Female", "Total Male")
        
        basic_2016_data <- as.data.frame(t(basic_2016_data))
        basic_2016_data <- tibble::rownames_to_column(basic_2016_data, " ")
        colnames(basic_2016_data) <- c(" ", " ")
        return(basic_2016_data)
    })
    
    output$p_table2_2016 <- renderTable({
        shiny::validate(need(input$school_profile != "", "Please choose a school for details."))
        shiny::validate(need(dim(sy_1617_data_react())[1] != 0, "No data available for selected school in school year 2016 - 2017"))
        basic_2016_data()
    })
    
    
    
    output$pwdChart_2016 <- renderHighchart({
        
        shiny::validate(need(input$school_profile != "", "Please choose a school for details."))
        shiny::validate(need(dim(sy_1617_data_react())[1] != 0, "No data available for selected school in school year 2015 - 2016"))
        shiny::validate(need(sy_1617_data_react()$pwd_total != 0, "No students with disabilites at selected school"))
        
        print(sy_1617_data_react())
        pwd_2016_vars <- c("ds_total",
                           "cp_total", "dcm_total", "drcpau_total", "dh_total",
                           "autism_total", "wcg_total", "eb_total", "hi_total",
                           "id_total", "li_total", "md_total", "pd_total",
                           "shp_total", "speech_total", "vi_total", "ii_total",                           
                           "p_total")
        colnames(pwd_2016_vars) <- c("Difficulty Seeing Manifestation",
                                     "Cerebral Palsy",
                                     "Difficulty Communicating Manifestation",
                                     "Difficulty Remembering, Concentrating, Paying Attention and Understanding based on Manifestation",
                                     "Difficulty Hearing Manifestation",
                                     "Autism Spectral Disorder",
                                     "Difficulty Walking, Climbing and Grasping",
                                     "Emotional-Behavioral Disorder",
                                     "Hearing Impairment",
                                     "Intellectual Impairment",
                                     "Learning Impairment",
                                     "Multiple Disabilities",
                                     "Orthopedic/Physical Disorder",
                                     "Special Health Problem/Chronic Illness",
                                     "Speech Disorder",
                                     "Visual Impairment Disorder",
                                     "Intellectual Impairment",
                                     "Orthopedic/Physical Disorder")
        pwd_2016_data <- sy_1617_data_react()[pwd_2016_vars]
        pwd_2016_data <- as.data.frame(t(pwd_2016_data))
        pwd_2016_data <- tibble::rownames_to_column(pwd_2016_data, "Disability")
        colnames(pwd_2016_data) <- c("Disability", "Value")
        pwd_2016_data <- pwd_2016_data[pwd_2016_data$Value > 0,]
        hc <- pwd_2016_data %>%
            hchart(type = "column", hcaes(x = Disability, y = Value)) %>%
            hc_xAxis(title = list(text = "Disability")) %>%
            hc_yAxis(title = list(text = "Value"))
        return(hc)
        
    })
    
    
    # School Profiles 2017 - 2018 ---------------------------------------------
    sy_1718_data_react <- reactive({
        sy1718 <- all_data[all_data$school_year == 2017,]
        sy1718 <- sy1718[sy1718$region == input$region_profile,]
        sy1718 <- sy1718[sy1718$division == input$division_profile,]
        sy1718 <- sy1718[sy1718$district == input$district_profile,]
        sy1718 <- sy1718[sy1718$school_name == input$school_profile,]
        print(sy1718)
    })
    
    
    profile_2017_data_react <- reactive({
        profile_2017_vars <- c('school_name', "school_id", "region",
                               "division", "district", 'shi_score', 'sni_percentile_text',
                               'remoteness_index', 'remoteness_cluster', 'remoteness_percentile_text', 'cct_percentage',
                               'student_teacher_ratio', 'student_classroom_ratio',
                               'original_water_boolean', 'original_internet_boolean',
                               'original_electricity_boolean')
        profile_2017_data <- sy_1718_data_react()[profile_2017_vars]
        
        colnames(profile_2017_data) <- c(
            'School Name',
            "School ID",
            "Region",
            "Division",
            "District",
            'SNI Score',
            'SNI Percentile',
            'Remoteness Index',
            'Remoteness Cluster',
            'Remoteness Percentile',
            'CCT Percentage',
            'Student Teacher Ratio',
            'Student Classroom Ratio',
            'Water Access',
            'Internet Access',
            'Electricity Access'
        )
        
        profile_2017_data <- as.data.frame(t(profile_2017_data))
        profile_2017_data <- tibble::rownames_to_column(profile_2017_data, " ")
        colnames(profile_2017_data) <- c(" ", " ")
        return(profile_2017_data)
    })
    
    output$snitable_profile_2017 <- renderTable({
        print(dim(sy_1718_data_react())[1])
        shiny::validate(need(input$school_profile != "", "Please choose a school for details."))
        shiny::validate(need(dim(sy_1718_data_react())[1] != 0, "No data available for selected school in school year 2017 - 2018"))
        profile_2017_data_react()
        
    })
    
    
    pie_react_2017 <- reactive({
        pie_2017_data <- c(sy_1718_data_react()$total_female, sy_1718_data_react()$total_male)
        print(pie_2017_data)
        return(pie_2017_data)
    })
    
    output$distPie_2017 <- renderPlot({
        shiny::validate(need(input$school_profile != "", "Please choose a school for details."))
        shiny::validate(need(dim(sy_1718_data_react())[1] != 0, "No data available for selected school in school year 2017 - 2018"))
        pie(pie_react_2017(), labels = c("Female", "Male"), col = c('#e6a6c7', '#347dc1'))#, radius = 4)#, main, col, clockwise)
    })
    
    
    basic_2017_data <- reactive({
        basic_2017_vars <- c("school_year", "school_name", "province", 
                             "municipality", "region", "district", 
                             "division", "total_enrollment", 
                             "total_female", "total_male")
        basic_2017_data <- sy_1718_data_react()[basic_2017_vars]
        print(head(basic_2017_data))
        
        colnames(basic_2017_data) <- c('School Year', "School Name", "Province",
                                       "Municipality", "Region", "District", 
                                       "Division", "Total Enrollment", 
                                       "Total Female", "Total Male")
        
        basic_2017_data <- as.data.frame(t(basic_2017_data))
        basic_2017_data <- tibble::rownames_to_column(basic_2017_data, " ")
        colnames(basic_2017_data) <- c(" ", " ")
        return(basic_2017_data)
    })
    
    output$p_table2_2017 <- renderTable({
        shiny::validate(need(input$school_profile != "", "Please choose a school for details."))
        shiny::validate(need(dim(sy_1718_data_react())[1] != 0, "No data available for selected school in school year 2017 - 2018"))
        basic_2017_data()
    })
    
    
    
    output$pwdChart_2017 <- renderHighchart({
        
        shiny::validate(need(input$school_profile != "", "Please choose a school for details."))
        shiny::validate(need(dim(sy_1718_data_react())[1] != 0, "No data available for selected school in school year 2017 - 2018"))
        shiny::validate(need(sy_1718_data_react()$pwd_total != 0, "No students with disabilites at selected school"))
        
        print(sy_1718_data_react())
        pwd_2017_vars <- c("ds_total",
                           "cp_total", "dcm_total", "drcpau_total", "dh_total",
                           "autism_total", "wcg_total", "eb_total", "hi_total",
                           "id_total", "li_total", "md_total", "pd_total",
                           "shp_total", "speech_total", "vi_total", "ii_total",                           
                           "p_total")
        pwd_2017_data <- sy_1718_data_react()[pwd_2017_vars]
        colnames(pwd_2017_vars) <- c("Difficulty Seeing Manifestation",
                                     "Cerebral Palsy",
                                     "Difficulty Communicating Manifestation",
                                     "Difficulty Remembering, Concentrating, Paying Attention and Understanding based on Manifestation",
                                     "Difficulty Hearing Manifestation",
                                     "Autism Spectral Disorder",
                                     "Difficulty Walking, Climbing and Grasping",
                                     "Emotional-Behavioral Disorder",
                                     "Hearing Impairment",
                                     "Intellectual Impairment",
                                     "Learning Impairment",
                                     "Multiple Disabilities",
                                     "Orthopedic/Physical Disorder",
                                     "Special Health Problem/Chronic Illness",
                                     "Speech Disorder",
                                     "Visual Impairment Disorder",
                                     "Intellectual Impairment",
                                     "Orthopedic/Physical Disorder")
        pwd_2017_data <- as.data.frame(t(pwd_2017_data))
        pwd_2017_data <- tibble::rownames_to_column(pwd_2017_data, "Disability")
        colnames(pwd_2017_data) <- c("Disability", "Value")
        pwd_2017_data <- pwd_2017_data[pwd_2017_data$Value > 0,]
        hc <- pwd_2017_data %>%
            hchart(type = "column", hcaes(x = Disability, y = Value)) %>%
            hc_xAxis(title = list(text = "Disability")) %>%
            hc_yAxis(title = list(text = "Value"))
        return(hc)
        
    })
    
    
    QueryDataReact <- reactive({
        
        basic_details <- c('school_id', 'school_name', 'region', 'district', 'division', 'province', 'municipality','latitude', 'longitude')
        
        keep_columns <- rlist::list.append(basic_details, input$columns)
        
        if ('pwds' %in% input$columns) {
            
            keep_columns <- rlist::list.append(basic_details, c("ds_total",
                                                                "cp_total", "dcm_total", "drcpau_total", "dh_total",
                                                                "autism_total", "wcg_total", "eb_total", "hi_total",
                                                                "id_total", "li_total", "md_total", "pd_total",
                                                                "shp_total", "speech_total", "vi_total", "ii_total",                           
                                                                "p_total", 'pwd_total'))
        }
        
        if ('total_enrollment' %in% input$columns) {
            
            keep_columns <- rlist::list.append(basic_details, c('total_enrollment', 'total_female', 'total_male'))
        }
        
        data_download <- all_data[keep_columns]
        
        print(data_download)
        
        if (input$FilterGeo == 'School Region') {
            data_download <- data_download[data_download$region %in% input$QueryRegion,]
        } else if (input$FilterGeo == 'School District') {
            data_download <- data_download[data_download$district %in% input$QueryDistrict,]
        } else if (input$FilterGeo == 'School Division') {
            data_download <- data_download[data_download$division %in% input$QueryDivision,]
        } else if (input$FilterGeo == 'School Province') {
            data_download <- data_download[data_download$province %in% input$QueryProvince,]
        } else if (input$FilterGeo == 'School Municipality') {
            data_download <- data_download[data_download$municipality %in% input$QueryMunicipality,]
        }
        
        return(data_download)
        
    })
    
    output$QueryTablePreview <- DT::renderDataTable(DT::datatable(data = QueryDataReact(), options = list(autoWidth = FALSE)))
    
    
    
    
    # Output: Download Country Climate CSV ------------------------------------
    output$QueryBuilder <- downloadHandler(
        
        filename = function() {
            
            paste('CMSDataDownload', '.csv', sep='')
            
        },
        
        content = function(file) {
            
            write.csv(QueryDataReact(), file)
            
        }
        
    )
    
    
    
})