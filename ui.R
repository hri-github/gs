shinyUI(pageWithSidebar(
  headerPanel("Matching officers to agencies"),
  
  # Input in sidepanel:
  sidebarPanel(
    tags$style(type='text/css', ".well { max-width: 20em; }"),  # width of well
    tags$head(
      tags$style(type="text/css", "select[multiple] { width: 100%; height:10em}"),
      tags$style(type="text/css", "select { width: 100%}"),
      tags$style(type="text/css", ".well {background-color: white; }"),
      tags$style(type="text/css", "input { width: 19em; max-width:100%}")
    ),
    # Select filetype:
    selectInput("readFunction", "Kindly upload Excel xlsx files only", c(
      # Base R:
      # "read_xls",
      "read_xlsx"
    )),
    fileInput("file1", "Upload Agency's Preferences:"),
    fileInput("file2", "Upload Officer's Preferences:"),
    # textInput("name","Dataset name:","Data"),
    p("Once both files are uploaded, matching will automatically occur."),
    p("When matching is complete, you will be redirected to the Warnings tab."),
    textOutput("dlready"),
    br(),
    uiOutput("dl1"),
    br(),
    uiOutput("dl2"),
    br(),
    p("(v1.0) For enquiries, please e-mail tyler_huang@psd.gov.sg"),
    br(),
    img(src="psd_logo.jpg", align="bottom", width=200, height=200)    
    #downloadLink('downloadDump', 'Download dump')
  ), # sidebarpanel
  
  # Main:
  mainPanel(
    fluidRow(
      column(width=12,
             tabsetPanel(id="mytabset",
               tabPanel("Loaded Data", value="loadtab",
                        br(),
                        p("Please upload your data in the sidebar. You may refresh your browser to reset the app."),
                        p("Your data should resemble the examples below. Agencies with multiple jobs are shown by A2.1 and A2.2"),
                        p("Agencies and officers may have different number of preferences, but try to ensure that there are as many officers as jobs."),
                        p("Agencies with no preferences will be excluded from the matching."),
                        p("Officers who were not chosen by any agency will be excluded from the matching."),
                        textOutput("path1"), textOutput("path2"),
                        column(5.5, tableOutput("table1")), column(5.5, tableOutput("table2"))
                        ),
               tabPanel( "Warnings", value="warntab",
                        br(), 
                        textOutput("chk"), br(), 
                        textOutput("chk2"), br(),
                        textOutput("chk2b"), br(),
                        textOutput("chk2c")
                        ),
               tabPanel("Favour Officers", value="offtab",
                        br(), p("For this output, Officers' preferences are favoured more than agencies'."), 
                        tableOutput("table3"), br(),
                        textOutput("tab3_atext"), br(),
                        tableOutput("table3a"), br(),
                        textOutput("tab3_btext"), br(),
                        tableOutput("table3b")
                        ),
               tabPanel("Favour Agencies", value="agtab",
                        br(), p("For this output, Agencies' preferences are favoured more than officers'."), 
                        tableOutput("table4"), br(),
                        textOutput("tab4_atext"), br(),
                        tableOutput("table4a"), br(),
                        textOutput("tab4_btext"), br(),
                        tableOutput("table4b")
                        ),
               tabPanel("Algorithm", value="algotab",
                        br(),
                        img(src="GSalgo.jpg", width=700),
                        br(),
                        p("Source: https://images.nature.com/full/nature-assets/nature/journal/v492/n7427/images/492054a-f1.2.jpg")
                        )
             )
      )
    )
  ) # mainPanel
))

# setwd("C:\\Users\\PSD HRI\\Desktop")
# shiny::runApp("UploadMatch")
# rsconnect::deployApp("Matching Phase 1B", appName="matching_phase_1b")
# WARNING: drop_upload works via rsconnect::deployApp() BUT not runApp()