library(ggplot2)
library(dplyr)
library(stringr)
library(knitr)
library(tidyr)
library(shiny)
library(DT)

#UI Side
ui<-shinyUI(navbarPage("PitchFX APP",
                       tabPanel("Isaac", titlePanel("UserInput"),
                                sidebarLayout(
                                  sidebarPanel(helpText("PitchFX Input"),
                                               selectInput(inputId="pitch_type",
                                                           label="Select Pitch Type",
                                                           choices=list("Fourseam Fastball"="Fourseam_FB_Wrangled",
                                                                        "Twoseam Fastball"="Zourseam_FB_Wrangled",
                                                                        "Cutter"="Aourseam_FB_Wrangled",
                                                                        "Splitter"="Bourseam_FB_Wrangled",
                                                                        "Change"="Courseam_FB_Wrangled",
                                                                        "Curve"="Dourseam_FB_Wrangled",
                                                                        "Slider"="Eourseam_FB_Wrangled",
                                                                        "Sinker"="Fourseam_FB_Wrangled")),
                                               textInput(inputId="pfx_x",
                                                         label="PFX_X",
                                                         value=0),
                                               textInput(inputId="pfx_z",
                                                         label="PFX_Z",
                                                         value=0),
                                               textInput(inputId="effective_speed",
                                                         label="Effective Velo",
                                                         value=90),
                                               textInput(inputId="release_speed",
                                                         label="Release Velo",
                                                         value=93),
                                               textInput(inputId="release_spin_rate",
                                                         label="Release Spin Rate",
                                                         value=2000),
                                               sliderInput(inputId="pick_md", 
                                                           label="Pick Mahalanobis Distance",
                                                           min = 0, max = 15,
                                                           value = 1.0, step = 0.1)
                                  ), 
                                  mainPanel(
                                    tabsetPanel(type="tabs",
                                                tabPanel("One", DT::dataTableOutput("pitchTable")),
                                                tabPanel("Two", plotOutput("plot"))
                                                )
                                  )
                                ))
                                
                       )
                       
)
#server
server<-shinyServer(function(input,output){

    pitch_data_for_cov<-reactive({
      Fourseam_FB_Wrangled[,1:5]
    })
    
    cov_matrix<-reactive({
      
      solve(cov(pitch_data_for_cov(), y=NULL, use="everything"))
    })
    
    pitch_data<-reactive({
      CovInv<-cov_matrix()
      Fourseam_FB_Wrangled%>%
        mutate(d1=pfx_x-as.numeric(input$pfx_x))%>%
        mutate(d2=pfx_z-as.numeric(input$pfx_z))%>%
        mutate(d3=effective_speed-as.numeric(input$effective_speed))%>%
        mutate(d4=release_speed-as.numeric(input$release_speed))%>%
        mutate(d5=release_spin_rate-as.numeric(input$release_spin_rate))%>%
        mutate(p1=d1*CovInv[1]+d2*CovInv[2]+d3*CovInv[3]+d4*CovInv[4]+d5*CovInv[5])%>%
        mutate(p2=d1*CovInv[6]+d2*CovInv[7]+d3*CovInv[8]+d4*CovInv[9]+d5*CovInv[10])%>%
        mutate(p3=d1*CovInv[11]+d2*CovInv[12]+d3*CovInv[13]+d4*CovInv[14]+d5*CovInv[15])%>%
        mutate(p4=d1*CovInv[16]+d2*CovInv[17]+d3*CovInv[18]+d4*CovInv[19]+d5*CovInv[20])%>%
        mutate(p5=d1*CovInv[21]+d2*CovInv[22]+d3*CovInv[23]+d4*CovInv[24]+d5*CovInv[25])%>%
        mutate(md2=(d1*p1+d2*p2+d3*p3+d4*p4+d5*p5))%>%
        mutate(md=(md2)^.5)%>%
        arrange(md)%>%
        filter(md<=input$pick_md)
      
    })
    output$pitchTable<-DT::renderDataTable({
      DT::datatable(data=pitch_data(), rownames=FALSE)
    }, server=TRUE)
    
})




shinyApp(ui=ui, server=server)

