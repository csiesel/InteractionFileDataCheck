
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(stringr)
library(data.table)
library(janitor)
library(tidyverse)
library(flexdashboard)
library(shiny)
library(lubridate)
library(DT)
library(wesanderson)
library(MetBrewer)
library(knitr)
library(shades)
library(ggrepel)
library(scales)
library(ggridges)
library(viridis)
library(tidytext)
`%!in%` = Negate(`%in%`)
options(timeout=30000)
options(shiny.maxRequestSize=500*1024^2)

source("themes.R")


# Define UI for application that draws a histogram
ui <- dashboardPage(
    header = dashboardHeader(title = "NCD Data Checks"),
    sidebar = dashboardSidebar(minified = TRUE,
                               tags$style("label{color: #FD9E28;"),
                               tags$style("a{font-weight: bold;"),
                               sidebarMenu(
                                   menuItem("Inputs", icon=icon("cogs"), startExpanded = TRUE, selected=TRUE,
                                            #textInput("user", "Please enter your username", value=""), br(),
                                            #passwordInput("pass", "Please enter your password", value=""), br(),
                                            textInput("cntry", "Please enter the country: ", value=""), br(),

                                            textAreaInput("res_files", "Please enter the URLs for the results files (separated by `, `): " ,
                                                          "",
                                                          resize = "vertical"), br(),

                                            textAreaInput("int_files", "Please enter the URLs for the interaction files (separated by `, `): " ,
                                                          "",
                                                          resize = "vertical"), br())#,
                                   # menuItem("Outputs", icon=icon("file"), startExpanded = TRUE,
                                   #          actionButton("view", "Click to view report"),
                                   #          downloadButton("report", "Click to download report"))
                               )),
    body = dashboardBody(customTheme,
        tabsetPanel(type="tabs",
                    tabPanel("Data Preview",
                             DT::DTOutput("result"),
                             DT::DTOutput("int")),

                    tabPanel("Contacts and Responses",
                             h3("The plots below show the time at which respondents are contacted (sent prompts) and the time at which they respond (enter an answer to a prompt)",
                                align="center"),
                             h4("All contacts should occur within the window set in Surveda.
                                Responses can extend beyond the time window in Surveda but the majority should fall within.",
                                align="center"),
                             plotOutput("cont_time_plot"),
                             br(),
                             plotOutput("resp_time_plot"),
                             br(),
                             downloadButton("cont_resp_d", "Download csv"),
                             DTOutput("cont_resp")),

                    tabPanel("Contact Attempts and Lag",
                             h3("The plot below shows the lag between contact attempts across mode(s)",
                                align="center"),
                             h4("The median lag (vertical line) should be close or equal to the settings set in Surveda.",
                                align="center"),
                             plotOutput("cont_lag_ridges"),
                             br(),
                             downloadButton("numcont2_d", "Download csv"),
                             DT::DTOutput("numcont2"),
                             br(),
                             h3("The plot below shows the mode sequencing of contacts for all respondents (top) and respondents by disposition (bottom)",
                                align="center"),
                             h4("The most frequently observed option (and the option for `Unknown Eligibility` respondents) should match the contact settings on Surveda.",
                                align="center"),
                             plotOutput("contact_mode_tile"),
                             br(),
                             downloadButton("cont_answer_valid_d", "Download csv"),
                             DT::DTOutput("cont_answer_valid"),
                             br(),
                             plotOutput("contact_mode_tile_dispo"),
                             br(),
                             h3("The plot below shows the percent of respondents who engaged with the survey within each contact attempt", align="center"),
                             h4("Engagement is calculated as the number of respondents who answered a contact attempt (answered IVR call, responded to SMS, responded to Mobile Web)
                                 divided by the total number of respondents contacted in that attempt.", align="center"),
                             plotOutput("cont_resp_rates")),

                    tabPanel("Errors",
                             h3("The plot below shows the percent and number of responses that resulted in errors by question",
                                align="center"),
                             h4("Questions with high proportions of responses that result in errors should be further investigated for refinement.",
                                align="center"),
                             plotOutput("errors_by_prompt"),
                             br(),
                             DT::DTOutput("error_responses")),

                    tabPanel("Breakoff",
                             h3("The plot below shows the questions at which respondents break off from the survey",
                                align="center"),
                             h4("Questions with high break off numbers should be investigated further for issues in wording or responses.",
                                align="center"),
                             plotOutput("breakoff_plot", height="800px"),
                             br(),
                             DT::DTOutput("breakoff_table")),

                    tabPanel("Report",
                             actionButton("view", "Click to view report"),
                             actionButton("report", "Click to download report"),
                             htmlOutput("markdown"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session){
    session$allowReconnect(TRUE)

    #### Get Inputs ####
    res_files <- reactive({input$res_files})
    int_files <-reactive({input$int_files})
    #-------------------------------------------------------------------------#


    #### Load Res Files ####
    res_df <- reactive({

        req(input$res_files)


        # if(!is.na(which(user_profs$country==input$cntry & user_profs$User==input$user & user_profs$Password==input$pass))){
        #     res_file_urls <- str_replace(unlist(strsplit(user_profs$res_file_urls[which(user_profs$country==input$cntry & user_profs$User==input$user)], ",")), " ", "")
        #     res_df <- list()
        #     for(i in res_file_urls){
        #         temp_df <- read.csv(i)
        #         temp_df$mno <- str_remove(temp_df$sample_file, "\\_[[:digit:]].*")
        #         temp_df <- temp_df %>% clean_names()
        #         res_df[[i]] = temp_df
        #     }
        #     res_df <- do.call(plyr::rbind.fill, res_df)
        #     res_df <- as.data.frame(res_df) %>% clean_names()
        #     return(res_df)
        #
        # }
        #else{
            res_file_urls <- str_replace(unlist(strsplit(res_files(), ",")), " ", "")
            res_df <- list()
            for(i in res_file_urls){
                temp_df <- read.csv(i)
                temp_df$mno <- str_remove(temp_df$sample_file, "\\_[[:digit:]].*")
                temp_df <- temp_df %>% clean_names()
                res_df[[i]] = temp_df
            }
            res_df <- do.call(plyr::rbind.fill, res_df)
            res_df <- as.data.frame(res_df) %>% clean_names()
            return(res_df)
        #}
    })

    output$result <- renderDT(head(res_df(), 1000) %>% datatable(options = list(scrollX=TRUE)) %>% formatStyle(columns = c("section_order", "date", "modes", "sample_file", "mno"), "white-space" = "nowrap"))
    #-------------------------------------------------------------------------#


    #### Load Int Files ####
    int_df <- reactive({

        req(input$res_files, input$int_files)

        int_file_urls <- str_replace(unlist(strsplit(int_files(), ",")), " ", "")
        int_df <- list()
        for(i in int_file_urls){
            temp_df <- read.csv(url(i))
            # temp_df$mno <- "ASDF" #str_remove(temp_df$sample_file, "\\_[[:digit:]].*")
            temp_df <- temp_df %>% clean_names()
            int_df[[i]] = temp_df
        }
        int_df <- do.call(plyr::rbind.fill, int_df)
        int_df <- as.data.frame(int_df) %>% clean_names()
        # int_df <- do.call(rbindlist, int_df)
        int_df <- merge(int_df, res_df() %>% dplyr::select("respondent_id", "mno"), by="respondent_id")
        return(int_df)
    })

    output$int <- renderDT(head(int_df(), 1000) %>% datatable(options = list(scrollX=TRUE)))
    #-------------------------------------------------------------------------#

    #### Manipulating Data ####
    temp_res <- reactive({

        req(res_df())

        temp_res <- res_df() %>%
        mutate(age = as.numeric(age)) %>%
        mutate(age_bin = case_when(age < 18 ~ "<18",
                                   age >=18 & age <30 ~ "18-29",
                                   age >=30 & age < 45 ~ "30-44",
                                   age >=45 ~ "45+")) %>%
        mutate(gender = gsub('[[:digit:]]+', '', gender)) %>%
        mutate(dispo = case_when(disposition %in% c("Completed", "Interim partial", "Partial") ~ "Complete",
                                 TRUE ~ disposition)) %>%
        mutate(date2 = format(as_datetime(date, format = "%b %d, %Y %H:%M"), format="%m %d %Y")) %>%
        mutate(dispo = case_when(disposition=="Completed" ~ "Completed",
                                 disposition=="Interim partial" ~ "Partial",
                                 disposition=="Partial" ~ "Partial",
                                 disposition=="Refused" ~ "Refused",
                                 disposition=="Rejected" ~ "Not Eligible",
                                 disposition=="Ineligible" ~ "Not Eligible",
                                 disposition=="Breakoff" & !is.na(gender) & !is.na(age) ~ "Eligible - Non-Interview",
                                 disposition=="Breakoff" & is.na(gender) | is.na(age) ~ "Unknown Eligiblity - Non-Interview",
                                 disposition=="Contacted" ~ "Unknown Eligiblity - Non-Interview",
                                 disposition=="Failed" ~ "Unknown Eligiblity - Non-Interview",
                                 disposition=="Started" & !is.na(gender) & !is.na(age) ~ "Eligible - Non-Interview",
                                 disposition=="Started" & is.na(gender) | is.na(age) ~ "Unknown Eligiblity - Non-Interview",
                                 disposition=="Unresponsive" ~ "Unknown Eligiblity - Non-Interview"))

        return(temp_res)
    })

    temp_int <- reactive({

        req(int_df())

        temp_int <- int_df() %>%
        # mutate(age = as.numeric(age)) %>%
        # mutate(age_cat = case_when(age < 18 ~ "<18",
        #                            age >=18 & age <30 ~ "18-29",
        #                            age >=30 & age < 45 ~ "30-44",
        #                            age >=45 ~ "45+")) %>%
        mutate(dispo = case_when(disposition %in% c("Completed", "Interim partial", "Partial") ~ "Complete",
                                 TRUE ~ disposition)) %>%
        mutate(date2 = format(as.POSIXct(timestamp), format="%m %d %Y"))

        return(temp_int)
    })


    #### Contacts and Response ####
    cont_resp <- reactive({
        req(temp_int())

        temp_contact <- temp_int() %>% filter(action_type %in% c("Contact attempt", "Contacted", "Prompt", "Response")) %>%
            dplyr::distinct(., respondent_id, mode, action_type, timestamp, .keep_all = TRUE) %>%
            mutate(time=as.POSIXct(timestamp)) %>%
            mutate_at(vars(contains('timestamp')), ~format(as.POSIXct(.), format="%H:%M")) %>%
            mutate(timestamp = hm(timestamp))

        contact <- temp_contact %>% as.data.frame() %>%
            merge(., temp_res() %>% select(respondent_id, gender, age), by="respondent_id", all.x=TRUE) %>%
            mutate(cat = "contact")

        response <- temp_contact %>% filter(action_type=="Response") %>%
            merge(temp_res() %>% select(respondent_id, gender, age), by="respondent_id") %>%
            mutate(cat = "response")

        cont_resp <- rbind(contact, response)
        return(cont_resp)
    })

    cont_time_plot <- reactive({cont_resp() %>%
        filter(cat=="contact") %>%
        # mutate(time = format(as.POSIXct(time), "%H:%M")) %>%
        ggplot(., aes(x=timestamp, fill=mno)) +
        geom_bar() +
        geom_density(aes(y=100*..count.., color=mno), fill=NA, size=1) +
        scale_fill_manual(values = wes_palette("Darjeeling1", n=6, type="continuous")) +
        scale_color_manual(values = brightness(wes_palette("Darjeeling1", n=6, type="continuous"), .4)) +
        scale_x_time(breaks = date_breaks('1 hour'), labels = time_format(format="%H:%M"), expand = c(0,0)) +
        labs(fill="Country",
             x="Time of Contact",
             y="Number of Respondents",
             title=paste0("Trend in MPS Contacts over Time")) +
        theme_minimal() +
        guides(fill="none", color="none") +
        theme(text = element_text(size=18),
              axis.text.x = element_text(angle=-90),
              axis.ticks.y = ) +
        facet_wrap(~mode, ncol=1, scales = "free_y")
    })

    output$cont_time_plot <- renderPlot({cont_time_plot()})

    resp_time_plot <- reactive({cont_resp() %>%
            filter(cat=="response") %>%
            # mutate(time = format(as.POSIXct(time), "%H:%M")) %>%
            ggplot(., aes(x=timestamp, fill=mno)) +
            geom_bar() +
            geom_density(aes(y=100*..count.., color=mno), fill=NA, size=1) +
            scale_fill_manual(values = wes_palette("Darjeeling1", n=6, type="continuous")) +
            scale_color_manual(values = brightness(wes_palette("Darjeeling1", n=6, type="continuous"), .4)) +
            scale_x_time(breaks = date_breaks('1 hour'), labels = time_format(format="%H:%M"), expand = c(0,0)) +
            labs(fill="Country",
                 x="Time of Response",
                 y="Number of Respondents",
                 title=paste0("Trend in MPS Responses over Time")) +
            theme_minimal() +
            guides(fill="none", color="none") +
            theme(text = element_text(size=18),
                  axis.text.x = element_text(angle=-90),
                  axis.ticks.y = ) +
            facet_wrap(~mode, ncol=1, scales = "free_y")})

    output$resp_time_plot <- renderPlot({resp_time_plot()})

    output$cont_resp <- DT::renderDT({cont_resp() %>% mutate(timestamp = format(timestamp, "%H:%M")) %>%
                                             datatable(options = list(scrollX=TRUE))})


    #### Contact Lag and Mode ####

    cont_answer_valid <- reactive({
        contacts <- temp_int() %>%
            mutate(dates = substr(timestamp, 1, 10),
                   dt = as.POSIXct(timestamp))

        #IVR
        cont_answer_IVR <- contacts %>%
            filter(mode=="IVR") %>%
            filter(action_type=="Contact attempt" & action_data!='User hangup') %>%
            filter(action_data!="Timeout") %>%
            filter(action_data!="Enqueueing call") %>%
            arrange(-id) %>%
            distinct(respondent_id, action_data, dates, .keep_all = TRUE)

        #SMS
        cont_answer_SMS <- contacts %>%
            filter(mode=="SMS") %>%
            group_by(respondent_id, mode, dates) %>%
            mutate(bad = ifelse(action_type[which.min(id)]=="Response", "bad", "good")) %>%
            ungroup() %>%
            filter(action_data!="Response") %>%
            filter(action_type!="Disposition changed") %>%
            mutate(row=row_number()) %>%
            group_by(respondent_id, mode, dates) %>%
            mutate(resp = case_when(any(action_type=="Response") ~ "Answer",
                                    TRUE ~ "no-answer")) %>%
            slice(which.max(row)) %>%
            arrange(-id) %>%
            distinct(respondent_id, action_data, dates, .keep_all = TRUE) %>%
            mutate(action_data=resp) %>%
            filter(bad !="bad") %>%
            dplyr::select(-bad, -resp, -row)

        #Mobile Web
        cont_answer_MW <- contacts %>%
            filter(mode=="Mobile Web", action_type != "Disposition changed", grepl("Contact|Lang", action_data)) %>%
            mutate(row=row_number()) %>%
            group_by(respondent_id, mode, dates) %>%
            mutate(resp = case_when(any(action_type=="Response") ~ "Answer",
                                    TRUE ~ "no-answer")) %>%
            slice(which.max(row)) %>%
            arrange(-id) %>%
            distinct(respondent_id, action_data, dates, .keep_all = TRUE) %>%
            mutate(action_data=resp) %>%
            dplyr::select(-resp, -row)


        cont_answer <- rbind(cont_answer_IVR, cont_answer_MW, cont_answer_SMS)

        cont_answer_valid <- cont_answer %>%
            distinct(respondent_id, dates, .keep_all = TRUE) %>%
            arrange(id) %>%
            group_by(respondent_id) %>%
            mutate(contact_attempt = paste0("attempt_", row_number()),
                   contact_mode = paste0("attempt_", row_number(), "_", mode)) %>%
            pivot_wider(id_cols=c("respondent_id"), names_from=c("contact_attempt"), values_from="mode") %>%
            merge(., temp_res() %>% select(respondent_id, dispo), by="respondent_id")

        return(cont_answer_valid)

    })

    numcont2 <- reactive({
        contacts <- temp_int() %>%
            mutate(dates = substr(timestamp, 1, 10),
                   dt = as.POSIXct(timestamp))

        #IVR
        cont_answer_IVR <- contacts %>%
            filter(mode=="IVR") %>%
            filter(action_type=="Contact attempt" & action_data!='User hangup') %>%
            filter(action_data!="Timeout") %>%
            filter(action_data!="Enqueueing call") %>%
            arrange(-id) %>%
            distinct(respondent_id, action_data, dates, .keep_all = TRUE)

        #SMS
        cont_answer_SMS <- contacts %>%
            filter(mode=="SMS") %>%
            group_by(respondent_id, mode, dates) %>%
            mutate(bad = ifelse(action_type[which.min(id)]=="Response", "bad", "good")) %>%
            ungroup() %>%
            filter(action_data!="Response") %>%
            filter(action_type!="Disposition changed") %>%
            mutate(row=row_number()) %>%
            group_by(respondent_id, mode, dates) %>%
            mutate(resp = case_when(any(action_type=="Response") ~ "Answer",
                                    TRUE ~ "no-answer")) %>%
            slice(which.max(row)) %>%
            arrange(-id) %>%
            distinct(respondent_id, action_data, dates, .keep_all = TRUE) %>%
            mutate(action_data=resp) %>%
            filter(bad !="bad") %>%
            dplyr::select(-bad, -resp)

        #Mobile Web
        cont_answer_MW <- contacts %>%
            filter(mode=="Mobile Web", action_type != "Disposition changed", grepl("Contact|Lang", action_data)) %>%
            mutate(row=row_number()) %>%
            group_by(respondent_id, mode, dates) %>%
            mutate(resp = case_when(any(action_type=="Response") ~ "Answer",
                                    TRUE ~ "no-answer")) %>%
            slice(which.max(row)) %>%
            arrange(-id) %>%
            distinct(respondent_id, action_data, dates, .keep_all = TRUE) %>%
            mutate(action_data=resp) %>%
            dplyr::select(-resp)


        a_data <- unique(c(cont_answer_IVR$action_data, cont_answer_MW$action_data, cont_answer_SMS$action_data))
        valid_cont <- a_data[a_data %!in% c("failed","Temporary failure (ISDN:41)","Network out of order (ISDN:38)",
                                            "Bearer capability not authorized (ISDN:57)","Protocol error, unspecified (ISDN:111)",
                                            "No route to destination (ISDN:3)","Bearer capability not available (ISDN:58)","Bearer capability not implemented (ISDN:65)",
                                            "Incompatible destination (ISDN:88)","Destination out of order (ISDN:27)","Number changed (ISDN:22)",
                                            "Switching equipment congestion (ISDN:42)","Interworking, unspecified (ISDN:127)","Circuit/channel congestion (ISDN:34)",
                                            "Invalid number format (ISDN:28)","Unallocated (unassigned) number (ISDN:1)","function_clause",
                                            "Facility not subscribed (ISDN:50)","Requested channel not available (ISDN:44)","Recover on timer expiry (ISDN:102)",
                                            "Channel not implemented (ISDN:66)","Facility rejected (ISDN:29)","timeout (undefined)",
                                            "Mandatory information element is missing (ISDN:96)", "Invalid message unspecified (ISDN:95)",
                                            "Call Rejected (ISDN:21)", "No user responding (ISDN:18)", "failed: Call Rejected (ISDN:21)")]

        # valid_cont <- a_data

        # Add this in the mutate for date lag:
        # date_lag = ifelse(respondent_id == lag(respondent_id, 1), paste0(lag(dates, 1), "  -  ", dates), NA)

        ivr <-cont_answer_IVR %>% filter(mode=="IVR") %>%
            filter(action_data %in% valid_cont) %>%
            distinct(respondent_id, dates, .keep_all = TRUE) %>%
            arrange(id) %>%
            group_by(respondent_id, mode) %>%
            mutate(contact_attempt = paste0("attempt_", row_number()),
                   contact_mode = paste0("attempt_", row_number(), "_", mode),
                   cont_lag = ifelse(respondent_id == lag(respondent_id, 1), difftime(dt, lag(dt, 1), units = "hours"), NA)) %>%
            pivot_wider(id_cols=c("respondent_id", "mode"), names_from=c("contact_mode"), values_from=c("action_data", "cont_lag"), names_glue="{contact_mode}_{.value}") %>%
            merge(., temp_res(), by="respondent_id") %>%
            mutate(dispo = case_when(disposition=="Completed" ~ "Completed",
                                     disposition=="Interim partial" ~ "Partial",
                                     disposition=="Partial" ~ "Partial",
                                     disposition=="Refused" ~ "Refused",
                                     disposition=="Rejected" ~ "Not Eligible",
                                     disposition=="Ineligible" ~ "Not Eligible",
                                     disposition=="Breakoff" & !is.na(gender) & !is.na(age) ~ "Eligible - Non-Interview",
                                     disposition=="Breakoff" & is.na(gender) | is.na(age) ~ "Unknown Eligiblity - Non-Interview",
                                     disposition=="Contacted" ~ "Unknown Eligiblity - Non-Interview",
                                     disposition=="Failed" ~ "Unknown Eligiblity - Non-Interview",
                                     disposition=="Started" & !is.na(gender) & !is.na(age) ~ "Eligible - Non-Interview",
                                     disposition=="Started" & is.na(gender) | is.na(age) ~ "Unknown Eligiblity - Non-Interview",
                                     disposition=="Unresponsive" ~ "Unknown Eligiblity - Non-Interview"))

        sms <- cont_answer_SMS %>% filter(mode=="SMS") %>%
            distinct(respondent_id, dates, .keep_all = TRUE) %>%
            arrange(id) %>%
            group_by(respondent_id, mode) %>%
            mutate(contact_attempt = paste0("attempt_", row_number()),
                   contact_mode = paste0("attempt_", row_number(), "_", mode),
                   cont_lag = ifelse(respondent_id == lag(respondent_id, 1), difftime(dt, lag(dt, 1), units = "hours"), NA)) %>%
            pivot_wider(id_cols=c("respondent_id", "mode"), names_from=c("contact_mode"), values_from=c("action_data", "cont_lag"), names_glue="{contact_mode}_{.value}") %>%
            merge(., temp_res(), by="respondent_id") %>%
            mutate(dispo = case_when(disposition=="Completed" ~ "Completed",
                                     disposition=="Interim partial" ~ "Partial",
                                     disposition=="Partial" ~ "Partial",
                                     disposition=="Refused" ~ "Refused",
                                     disposition=="Rejected" ~ "Not Eligible",
                                     disposition=="Ineligible" ~ "Not Eligible",
                                     disposition=="Breakoff" & !is.na(gender) & !is.na(age) ~ "Eligible - Non-Interview",
                                     disposition=="Breakoff" & is.na(gender) | is.na(age) ~ "Unknown Eligiblity - Non-Interview",
                                     disposition=="Contacted" ~ "Unknown Eligiblity - Non-Interview",
                                     disposition=="Failed" ~ "Unknown Eligiblity - Non-Interview",
                                     disposition=="Started" & !is.na(gender) & !is.na(age) ~ "Eligible - Non-Interview",
                                     disposition=="Started" & is.na(gender) | is.na(age) ~ "Unknown Eligiblity - Non-Interview",
                                     disposition=="Unresponsive" ~ "Unknown Eligiblity - Non-Interview"))

        mw <- cont_answer_MW %>% filter(mode=="Mobile Web") %>%
            distinct(respondent_id, dates, .keep_all = TRUE) %>%
            arrange(id) %>%
            group_by(respondent_id, mode) %>%
            mutate(contact_attempt = paste0("attempt_", row_number()),
                   contact_mode = paste0("attempt_", row_number(), "_", mode),
                   cont_lag = ifelse(respondent_id == lag(respondent_id, 1), difftime(dt, lag(dt, 1), units = "hours"), NA)) %>%
            pivot_wider(id_cols=c("respondent_id", "mode"), names_from=c("contact_mode"), values_from=c("action_data", "cont_lag"), names_glue="{contact_mode}_{.value}") %>%
            merge(., temp_res(), by="respondent_id") %>%
            mutate(dispo = case_when(disposition=="Completed" ~ "Completed",
                                     disposition=="Interim partial" ~ "Partial",
                                     disposition=="Partial" ~ "Partial",
                                     disposition=="Refused" ~ "Refused",
                                     disposition=="Rejected" ~ "Not Eligible",
                                     disposition=="Ineligible" ~ "Not Eligible",
                                     disposition=="Breakoff" & !is.na(gender) & !is.na(age) ~ "Eligible - Non-Interview",
                                     disposition=="Breakoff" & is.na(gender) | is.na(age) ~ "Unknown Eligiblity - Non-Interview",
                                     disposition=="Contacted" ~ "Unknown Eligiblity - Non-Interview",
                                     disposition=="Failed" ~ "Unknown Eligiblity - Non-Interview",
                                     disposition=="Started" & !is.na(gender) & !is.na(age) ~ "Eligible - Non-Interview",
                                     disposition=="Started" & is.na(gender) | is.na(age) ~ "Unknown Eligiblity - Non-Interview",
                                     disposition=="Unresponsive" ~ "Unknown Eligiblity - Non-Interview"))


        num_cont <- ivr %>% select(respondent_id, mno, dispo, matches("attempt_")) %>%
            merge(., sms %>% select(respondent_id, mno, dispo, matches("attempt_")),
                  by=c("respondent_id", "dispo", "mno"), all=TRUE) %>%
            merge(., mw %>% select(respondent_id, mno, dispo, matches("attempt_")),
                  by=c("respondent_id", "dispo", "mno"), all=TRUE) %>%
            mutate(contact_attempts = length(which(grepl("action_data", names(.))))-rowSums(is.na(.[,which(grepl("action_data", names(.)))])))

        numcont2 <- num_cont %>% group_by(respondent_id, mno) %>% mutate(contact_attempts=sum(contact_attempts, na.rm=TRUE)) %>% suppressWarnings(summarise_all(funs(max(as.character(.), na.rm=TRUE))))

        numcont2 <- numcont2 %>% select(-matches("5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|21"))

        return(numcont2)

    })

    output$cont_resp_rates <- renderPlot({
        resp_rates <- numcont2() %>%
            ungroup() %>%
            select(matches("action")) %>%
            # summarise_at(c("attempt_1_IVR_action_data"), list(length=length(which(.=="Answer"))))
            # summarise(groups=names(.)[which(grepl("action", names(.)))], n())
            summarise_all(.funs = ~length(which(.=="Answer"))/(length(which(!is.na(.))))*100)

        names(resp_rates) <- str_replace(names(resp_rates),"_action_data","")

        ggplot(pivot_longer(resp_rates, cols=everything()), aes(x=name, y=value, color=str_replace_all(names(resp_rates), c("attempt_|[[:digit:]]|_"),""))) +
            geom_point(size=5) +
            geom_text(aes(label=paste0(round(value,1), "%")), nudge_x=.3, size=10, show.legend=FALSE) +
            scale_color_manual(values=met.brewer("Veronese", n=5, type="continuous")) +
            labs(y="Percent Responsive\n(# Answered/# Contacted)",
                 x="Contact Attempt and Mode",
                 color="Mode") +
            theme_minimal() +
            theme(text = element_text(size=18),
                  axis.text.x = element_text(angle=-45, hjust=0))


    })

    cont_lag_ridges <- reactive({
        text_df <- numcont2() %>% ungroup() %>% select(respondent_id, (matches("lag") & !matches("date"))) %>%
            pivot_longer(cols = matches("lag")) %>% filter(!is.na(value)) %>%
            group_by(name) %>% summarise(value = median(value, na.rm=T))


        cont_lag_ridges <- ggplot(numcont2() %>% ungroup() %>% select(respondent_id, (matches("lag") & !matches("date"))) %>% pivot_longer(cols = matches("lag")) %>% filter(!is.na(value)),
                                  aes(x=value, y=name)) +
            ggridges::stat_density_ridges(geom="density_ridges_gradient", aes(fill=factor(stat(quantile))), calc_ecdf=TRUE, scale=.9, quantile_lines=TRUE, quantiles=c(0.025,0.5, 0.975), jittered_points=FALSE, position=ggridges::position_points_jitter(width=0.5, height=0), point_shape="|", point_size=1, point_alpha=1, alpha=0.8) +
            geom_text(data=text_df, aes(label=round(value,2)), position = position_nudge(y=-0.1)) +
            scale_fill_manual(
                name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#A0A0A0A0", "#0000FFA0"),
                labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.025, 0.975]", "(0.975, 1]")
            ) +
            guides(fill="none") +
            labs(caption = "Red and blue areas indicate 0-0.025 and 0.975-1 probability.\nVertical line indicates median.",
                 x="Time between contact attempts (hours)",
                 y="Current contact attempt",
                 subtitle = "Lag between contact attempts and the previous contact attempt") +
            theme_minimal() +
            theme(text = element_text(size=18),
                  plot.caption = element_text(hjust=0.5))

        return(cont_lag_ridges)
    })

    output$cont_lag_ridges <- renderPlot({cont_lag_ridges()})

    output$numcont2 <- DT::renderDT({numcont2() %>%
                                            datatable(options = list(scrollX=TRUE))})

    contact_mode_tile <- reactive({
        contact_by_mode <- cont_answer_valid() %>%
            ungroup() %>%
            select(contains("attempt")) %>%
            group_by_all() %>%
            summarise(n=n()) %>%
            ungroup() %>%
            mutate(row_num=paste0("Option ", row_number())) %>%
            pivot_longer(!c(row_num, n), names_to="attempt") %>%
            mutate(n=ifelse(is.na(value), NA, n))

        # Remove the filter below to show all options
        contact_mode_tile <- ggplot(contact_by_mode,
                                    aes(x=attempt, y=reorder(row_num, n), fill=n, text=ifelse(is.na(value), "", paste0(value, "\n", n)),
                                        label=ifelse(is.na(value), "", paste0(value, "\n", n)))) +
            geom_tile(color="white") +
            geom_text(color="white", fontface="bold") +
            scale_fill_gradientn(colors=met.brewer("Hokusai2", type="continuous"), na.value="transparent") +
            scale_y_reordered() +
            theme_minimal() +
            guides(fill="none")
        return(contact_mode_tile)
    })

    output$contact_mode_tile <- renderPlot({contact_mode_tile()})

    contact_mode_tile_dispo <- reactive({
        contact_by_mode <- cont_answer_valid() %>%
            ungroup() %>%
            select(contains("attempt"), dispo) %>%
            group_by_all() %>%
            summarise(n=n()) %>%
            ungroup() %>%
            mutate(row_num=paste0("Option ", row_number())) %>%
            pivot_longer(!c(row_num, n, dispo), names_to="attempt") %>%
            mutate(n=ifelse(is.na(value), NA, n))

        # Remove the filter below to show all options
        contact_mode_tile_dispo <- ggplot(contact_by_mode,
                                    aes(x=attempt, y=reorder_within(row_num, n, list(dispo)), fill=n, text=ifelse(is.na(value), "", paste0(value, "\n", n)),
                                        label=ifelse(is.na(value), "", paste0(value, "\n", n)))) +
            geom_tile(color="white") +
            geom_text(color="white", fontface="bold") +
            scale_fill_gradientn(colors=met.brewer("Hokusai2", type="continuous"), na.value="transparent") +
            scale_y_reordered() +
            theme_minimal() +
            guides(fill="none") +
            facet_wrap(~dispo, scales = "free_y")
        return(contact_mode_tile_dispo)
    })

    output$contact_mode_tile_dispo <- renderPlot({contact_mode_tile_dispo()})

    output$cont_answer_valid <- DT::renderDT({cont_answer_valid() %>%
            datatable(options = list(scrollX=TRUE))})




    #### Errors ####

    errors_df2 <- reactive({

        temp_prompt2 <- temp_int() %>%
            filter(action_type !="Contact attempt") %>%
            filter(action_type !="Disposition changed")


        responses2 <- temp_prompt2$action_data[which(temp_prompt2$action_type=="Response")]
        prompts2 <- temp_prompt2$action_data[which(temp_prompt2$action_type=="Response")-1]
        ids2 <- temp_prompt2$respondent_id[which(temp_prompt2$action_type=="Response")]
        mode2 <- temp_prompt2$mode[which(temp_prompt2$action_type=="Response")]
        errors2 <- temp_prompt2$action_data[which(temp_prompt2$action_type=="Response") + 1]



        unique_prompts2 <- trimws(unique(gsub('[[:digit:]]+', "", temp_prompt2$action_data[which(temp_prompt2$action_type=="Prompt")])))


        taco2<-data.frame("prompt"=prompts2, "response"=responses2, "id"=ids2, "mode"=mode2, "errors"=errors2) %>%
            mutate(prompt = trimws(gsub('[[:digit:]]+', "", prompt)),
                   errors = trimws(gsub('[[:digit:]]+', "", errors)))


        errors_df2 <- taco2 %>%
            filter(prompt %!in% c("Password or Format wrong!", "UNKNOWN CMD"),
                   response %!in% c("Password or Format wrong!", "UNKNOWN CMD")) %>%
            filter(prompt %in% unique_prompts2) %>%
            # Standardizing prompts
            mutate(prompt = tolower(prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("days eat fruits", "days of fruit", "number of days eat fruit"), "days eat fruit", prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("number of days eat veg", "days of vegetables"), "days eat vegetables", prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("servings of veg per day"), "servings of vegetables per day", prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("share phone numbers"), "share phone number", prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("servings of fruits per day"), "servings of fruit per day", prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("salt added while food is prepared at home", "salt in food and home"), "salt in food at home", prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("processed foods", "processed food"), "eat processed foods", prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("drugs: hypertension"), "drugs: high blood pressure", prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("ever dx diabetes"), "have diabetes", prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("ever dx hypertension", "diagnosed with high blood pressure"), "have high blood pressure", prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("heavy episodic drinking"), "heavy drinking", prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("intro & consent", "introduction and consent b", "introduction and consent ", "introduction & consent"), "introduction and consent", prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("electronic cigarette smoking", "current smokeless"), "smokeless tobacco", prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("current alcohol", "drink alcohol past  days"), "drink any alcohol", prompt)) %>%
            mutate(prompt = ifelse(prompt %in% c("current smoking"), "currently smoke", prompt)) %>%
            # mutate(prompt = ifelse(prompt %in% c("", ""), "", prompt)) %>%
            filter(prompt %!in% c("complete message", "contact", "flag: consent", "ineligible message", "ineligible mssg", "refusal msg", "quota completed", "refusal message", "thank you")) %>%
            mutate(prompt = factor(prompt, levels=c("language selection", "introduction and consent", "gender",
                                                    "age", "education", "currently smoke", "electronic cigarette smoking",
                                                    "smokeless tobacco", "drink any alcohol", "heavy drinking",
                                                    "days eat fruit", "servings of fruit per day",
                                                    "different types of fruits (new moh question)", "days eat vegetables", "servings of vegetables per day",
                                                    "different types of vegetables (new moh question)",
                                                    "add salt", "salt in food at home", "eat processed foods",
                                                    "have high blood pressure", "drugs: high blood pressure", "have diabetes", "drugs: diabetes",
                                                    "phone numbers", "share phone number",
                                                    "review graphic nutritional system", "days drink sugary drinks",
                                                    "days drink diet or light drinks", "days of mental health stress", "physical activity per week",
                                                    "covid- drinking", "covid- smoking", "residence", "physical activity"))) %>%
            # ADDED THIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            mutate(errors = case_when(prompt %in% c("language selection") & !grepl("\\D", response) ~ "Good",
                                      prompt %in% c("language selection") & grepl("\\D", response) ~ "Error",
                                      prompt %in% c("introduction and consent", "gender") & grepl("^1|^3", response) ~ "Good",
                                      prompt %in% c("introduction and consent", "gender") & !grepl("^1|^3", response) ~ "Error",
                                      prompt %in% c("education") & response %in% c("#",1,2,3,4,5,6,7,8) ~ "Good",
                                      prompt %in% c("education") ~ "Error",
                                      prompt %in% c("age") & as.numeric(response)>0 & as.numeric(response)<=120 ~ "Good",
                                      prompt %in% c("age") & (as.numeric(response)<=0 | as.numeric(response)>120) | prompt %in% c("age") & is.na(as.numeric(response)) ~ "Error",
                                      prompt %in% c("currently smoke", "electronic cigarette smoking", "smokeless tobacco") & response %in% c("#", 1, 2, 3) ~ "Good",
                                      prompt %in% c("currently smoke", "electronic cigarette smoking", "smokeless tobacco") & response %!in% c("#", 1, 2, 3) ~ "Error",
                                      prompt %in% c("drink any alcohol") & response %in% c("#", 1, 3) ~ "Good",
                                      prompt %in% c("drink any alcohol") & response %!in% c("#", 1, 3) ~ "Error",
                                      prompt %in% c("heavy drinking") & as.numeric(response) <=75 | prompt %in% c("heavy drinking") & as.numeric(gsub("([0-9]+).*$", "\\1", response))<=75 ~ "Good",
                                      prompt %in% c("heavy drinking")  ~ "Error",
                                      prompt %in% c("days eat fruit", "days eat vegetables") & (response=="#" | grepl("^[[:digit:]]+", response) & as.numeric(substr(response,1,1))<=7)  ~ "Good",
                                      prompt %in% c("days eat fruit", "days eat vegetables") ~ "Error",
                                      prompt %in% c("servings of fruit per day", "servings of vegetables per day") & (response=="#" | grepl("^[[:digit:]]+", response) & as.numeric(substr(response,1,1))<=50)  ~ "Good",
                                      prompt %in% c("servings of fruit per day", "servings of vegetables per day") ~ "Error",
                                      prompt %in% c("add salt") & response %in% c("#", 1,2,3,4,5,7) ~ "Good",
                                      prompt %in% c("add salt") ~ "Error",
                                      prompt %in% c("salt in food at home") & response %in% c("#",1,2,3,4,5,7) ~ "Good",
                                      prompt %in% c("eat processed foods") & response %in% c("#",1,2,3,4,5) ~ "Good",
                                      prompt %in% c("salt in food at home", "eat processed foods") ~ "Error",
                                      prompt %in% c("have high blood pressure", "drugs: high blood pressure",
                                                    "have diabetes", "drugs: diabetes") & response %in% c("#", 1, 3) ~ "Good",
                                      prompt %in% c("have high blood pressure", "drugs: high blood pressure",
                                                    "have diabetes", "drugs: diabetes") ~ "Error",
                                      (prompt %in% c("phone numbers", "share phone number") & response=="#") | (prompt %in% c("phone numbers", "share phone number") & !is.na(as.numeric(response)) & as.numeric(response)<=99 & as.numeric(response)>=1) ~ "Good",
                                      prompt %in% c("phone numbers", "share phone number") ~ "Error",
                                      prompt %in% c("physical activity") & response %in% c("#",0,1,2,3,4,5,6,7) ~ "Good",
                                      prompt %in% c("physical activity") ~ "Error")) %>%
            filter(prompt %!in% c("electronic cigarette smoking", "different types of fruits (new moh question)",
                                  "different types of vegetables (new moh question)",
                                  "review graphic nutritional system", "days drink sugary drinks",
                                  "days drink diet or light drinks", "days of mental health stress", "physical activity per week",
                                  "covid- smoking", "covid- drinking", "residence")) %>%
            mutate(row_num=row_number()) %>%
            rename("respondent_id"="id") %>%
            merge(., temp_res(), by="respondent_id", all.x=TRUE) %>%
            group_by(respondent_id) %>%
            mutate(num_errors = length(which(errors=="Error"))) %>%
            ungroup() %>%
            arrange(row_num)

        return(errors_df2)
    })

    errors_by_prompt <- reactive({
        ggplot(errors_df2() %>% group_by(prompt) %>% summarise(n_error=length(which(errors=="Error")),
                                                               n_response = n(),
                                                               perc_error = n_error/n_response*100), aes(x=prompt, fill=perc_error, y="")) +
            geom_tile(color="white") +
            geom_text(aes(label=paste0(round(perc_error, 1), "%\n\n")), color="grey9", fontface="bold") +
            geom_text(aes(label=paste0("\n\n", n_error)), color="grey25") +
            scale_fill_gradientn(colours= wesanderson::wes_palette("Zissou1", n=3, type="continuous")) +
            scale_y_discrete(expand=c(0,0)) +
            scale_x_discrete(expand=c(0,4)) +
            guides(fill="none") +
            labs(x="Question w/ Entry Error",
                 y="",
                 title=paste0("Percentage of Response Errors by Survey Question")) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle=-45, vjust=.5, hjust=0),
                  panel.grid.major.x=element_blank(),
                  text = element_text(size=18))
    })

    output$errors_by_prompt <- renderPlot({errors_by_prompt()})

    error_responses <- reactive({
        error_responses <- errors_df2() %>%
            filter(errors=="Error") %>%
            group_by(prompt, response) %>%
            summarise(n=n()) %>%
            group_by(prompt) #%>%
            #slice_max(order_by=n, n=5, with_ties = FALSE)
        return(error_responses)
    })

    output$error_responses <- DT::renderDT({error_responses() %>% datatable(options = list(scrollX=TRUE))})


    #### Breakoff ####
    breakoff_prompt <- reactive({temp_int() %>%
            filter(action_type=="Prompt") %>%
            filter(action_data!="Error") %>%
            group_by(respondent_id) %>%
            slice(which.max(id)) %>%
            filter(disposition %!in% c("Contacted", "Ineligible", "Refused", "Completed")) %>%
            mutate(action_data = gsub('[[:digit:]]',"", action_data)) %>%
            mutate(action_data = gsub(" ", "", action_data)) %>%
            filter(action_data !="Thankyou")
    })

    breakoff_plot <- reactive({
        ggplot(breakoff_prompt() %>%
                   mutate(action_data = str_replace(action_data, "(new MOH question)", "")) %>%
                   group_by(action_data) %>%
                   summarise(n=n()) %>%
                   arrange(-n),
               aes(x=reorder(action_data, -n), y=log(n)+.75, label=n)) +
            geom_col() +
            geom_text(position=position_stack(vjust=0.5), angle=-90, size=16, color="white") +
            labs(y="") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle=-90, vjust=.5, hjust=0),
                  panel.grid.major.x=element_blank(),
                  text = element_text(size=18), axis.text.y = element_blank())
    })

    output$breakoff_plot <- renderPlot({breakoff_plot()}, height=750)

    output$breakoff_table <- DT::renderDT({breakoff_prompt() %>% datatable(options = list(scrollX=TRUE))})


    #### Generate Report ####

    observeEvent(input$report, {
        params = list("cntry" = input$cntry,
                      "cont_time_plot" = cont_time_plot(),
                      "resp_time_plot" = resp_time_plot(),
                      "cont_lag_ridges" = cont_lag_ridges(),
                      "contact_mode_tile" = contact_mode_tile(),
                      "errors_by_prompt" = errors_by_prompt(),
                      "error_responses" = error_responses(),
                      "breakoff_plot" = breakoff_plot()
        )
        tempReport <- file.path(tempdir(), "NCDMPSDataQualityCheck.Rmd")
        file.copy("NCDMPSDataQualityCheck.Rmd", tempReport, overwrite=TRUE)
        outfile <- file.path(tempdir(), paste0(params$cntry, "_report_", Sys.Date()))

        rmarkdown::render(input=tempReport, output_file = outfile,
                          params = params)

    })

    # output$report <- downloadHandler(
    #     filename = paste0("NCD_DATA_CHECK_", Sys.Date(), "_report.html"),
    #     content = function(file){
    #         # SHINY VERSION
    #         # tempReport <- file.path(tempdir(), "NCDMPSDataQualityCheckFORSHINY.Rmd")
    #         # file.copy("NCDMPSDataQualityCheckFORSHINY.Rmd", tempReport, overwrite=TRUE)
    #         # LOCAL VEARSION
    #         tempReport <- file.path(tempdir(), "NCDMPSDataQualityCheck.Rmd")
    #         file.copy("NCDMPSDataQualityCheck.Rmd", tempReport, overwrite=TRUE)
    #
    #         params = list("cntry" = isolate(input$cntry),
    #                       "cont_time_plot" = isolate(cont_time_plot()),
    #                       "resp_time_plot" = isolate(resp_time_plot()),
    #                       "cont_lag_ridges" = isolate(cont_lag_ridges()),
    #                       "contact_mode_tile" = isolate(contact_mode_tile()),
    #                       "errors_by_prompt" = isolate(errors_by_prompt()),
    #                       "error_responses" = isolate(error_responses()),
    #                       "breakoff_plot" = isolate(breakoff_plot())
    #         )
    #
    #         rmarkdown::render(tempReport, output_file = file,
    #                           params = params,
    #                           envir = new.env(parent = globalenv()))
    #     }
    # )
    observeEvent(input$view, {
        params = list("cntry" = input$cntry,
                      "cont_time_plot" = cont_time_plot(),
                      "resp_time_plot" = resp_time_plot(),
                      "cont_lag_ridges" = cont_lag_ridges(),
                      "contact_mode_tile" = contact_mode_tile(),
                      "errors_by_prompt" = errors_by_prompt(),
                      "error_responses" = error_responses(),
                      "breakoff_plot" = breakoff_plot()
                      )
        output$markdown <- renderUI({
            includeHTML(
                rmarkdown::render(
                    # input="NCDMPSDataQualityCheckFORSHINY.Rmd",
                    input="NCDMPSDataQualityCheck.Rmd",
                    params = params
                )
            )
        })
    })

    #### Downloads ####
    output$cont_resp_d <- downloadHandler(
        filename = function(){
            paste0("cont_resp_", Sys.Date(), ".csv")
        },
        content = function(file){
            write.csv(cont_resp(), file)
        }
    )

    output$numcont2_d <- downloadHandler(
        filename = function(){
            paste0("cont_lag_", Sys.Date(), ".csv")
        },
        content = function(file){
            write.csv(numcont2(), file)
        }
    )

    output$cont_answer_valid_d <- downloadHandler(
        filename = function(){
            paste0("cont_mode_flow_", Sys.Date(), ".csv")
        },
        content = function(file){
            write.csv(cont_answer_valid(), file)
        }
    )

}

# Run the application
shinyApp(ui = ui, server = server)
