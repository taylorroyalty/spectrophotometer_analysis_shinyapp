#######
#Shiny app for analyzing ocean optics spectrophotometer
#######
library(shiny)
library(zoo)
library(DT)
library(gridExtra)
library(GMD)
library(ggdendro)
library(tidyverse)
library(ggdendro)
library(shinyjs)
library(shinyFiles)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }
             "
      )
    )
  ),
  useShinyjs(),
  headerPanel("Spectrophotometer Analysis GUI"),
  br(),
  br(),
  tabsetPanel(id = "main_tab",
              tabPanel("Configuration",
                       sidebarPanel(width = 7,
                                    tabsetPanel(
                                      tabPanel("General Parameters",
                                               br(),
                                               fluidRow(
                                                 headerPanel("Directory Input"),
                                                 column(width = 4,
                                                        shinyDirButton("dir", "Select Directory", "Upload")
                                                 ),
                                                 column(width = 8,
                                                        verbatimTextOutput("dir", placeholder = TRUE)
                                                 )
                                               ),
                                               br(),
                                               br(),
                                               radioButtons("data_type", "Select Spectra Type", c("Raw Spectra (requires sample/reference spectra)", "Pre-analyzed (e.g., absorbance)"), inline = TRUE),
                                               fluidRow(
                                                 column(width = 2,
                                                        div(
                                                          textInput(inputId = "intensity_units","Spectra Units",value = "Absorption"),
                                                          style="font-size:80%;"
                                                        )
                                                 ),
                                                 column(width = 2,
                                                        div(
                                                          textInput(inputId = "sample_pattern","Sample Pattern"),
                                                          style="font-size:80%;"
                                                        )
                                                 ),
                                                 column(width = 2,
                                                        div(
                                                          textInput(inputId = "reference_pattern","Reference Pattern"),
                                                          style="font-size:80%;"
                                                        )
                                                 ),
                                                 column(width = 2, 
                                                        div(
                                                          numericInput(inputId= "id_interval","Samples per Reference",value = 1,min=1,step=1),
                                                          style="font-size:80%;"
                                                        )
                                                 ),
                                                 column(width = 2, 
                                                        div(
                                                          numericInput(inputId= "path_len","Pathlength (m)",value = 0.01),
                                                          style="font-size:80%;"
                                                        )
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 1,
                                                        actionButton("load_files", "Load/Map Files")
                                                 )
                                               ),
                                               br(),
                                               br(),
                                               fluidRow(
                                                 headerPanel("Wavelength Range"),
                                                 column(width = 6,
                                                        numericInput(inputId="low.wavelength","Lower Wavelength Threshold (nm)",value=200)
                                                 ),
                                                 column(width = 6,
                                                        numericInput(inputId="high.wavelength","Upper Wavelength Threshold (nm)",value=800)
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 3,
                                                        numericInput(inputId = "smooth_window","Running Mean Window Size", value = 1, min = 1, step = 1)
                                                 ),
                                                 column(width = 3, offset = 3,
                                                        br(),
                                                        div(
                                                          checkboxInput(inputId = "write.file", "Write Output Files", FALSE),
                                                          style="font-size:150%;"
                                                        )
                                                 ),
                                                 column(width = 3, 
                                                        textInput(inputId = "write.header", "File Name Prefix")
                                                 )
                                               ),
                                               br(),
                                               br(),
                                               actionButton(inputId = "analyze_table", "Analyze Spectra")
                                      ),
                                      tabPanel("Spectral Slope",
                                               br(),
                                               checkboxInput("spec.slope", "Perform Analysis", FALSE),
                                               fluidRow(
                                                 column(width = 4,
                                                        headerPanel("Slope 1"),
                                                        column(width = 12,
                                                               numericInput(inputId="s1.lower", "Lower Wavelength (nm)",value=275)
                                                        ),
                                                        column(width = 12,
                                                               numericInput(inputId="s1.upper", "Upper Wavelength (nm)",value=295)
                                                        )
                                                 ),
                                                 column(width = 4,
                                                        headerPanel("Slope 2"),
                                                        column(width = 12,
                                                               numericInput(inputId="s2.lower", "Lower Wavelength (nm)",value=350)
                                                        ),
                                                        column(width = 12,
                                                               numericInput(inputId="s2.upper", "Upper Wavelength (nm)",value=400)
                                                        )
                                                 )
                                               )
                                      ),
                                      tabPanel("Hierarchical Clustering",
                                               br(),
                                               checkboxInput("cluster", "Perform Analysis", FALSE),
                                               fluidRow(
                                                 column(width = 4,
                                                        radioButtons("cluster_norm", "Normalize Spectra for Clustering", c("Yes", "No"), inline = TRUE)
                                                 ),
                                                 column(width = 4,
                                                        numericInput(inputId="cluster.threshold", "Cluster Threshold",value=0.05,max = 1)
                                                 )
                                               )
                                      ),
                                      tabPanel("Integrate Spectra",
                                               br(),
                                               checkboxInput("area.integrate", "Perform Analysis", FALSE),
                                               fluidRow(
                                                 column(width = 4, 
                                                        numericInput(inputId= "min.integrate","Lower Bound (nm)",value = 200)
                                                 ),
                                                 column(width = 4, 
                                                        numericInput(inputId= "max.integrate","Upper Bound (nm)",value = 800) 
                                                 )
                                               )
                                      ),
                                      tabPanel("Inspect Spectra",
                                               br(),
                                               checkboxInput("inspect_spectra", "Perform Analysis", FALSE),
                                               radioButtons("inspect_normalize", "Normalize Spectra for Inspection", c("Yes", "No"), inline = TRUE)
                                      )
                                    )
                       ),
                       
                       mainPanel(
                         fluidRow(
                           column(width = 6, "Reference Spectra Table", DT::dataTableOutput("fileList_reference")
                           ),
                           column(width = 6,"Sample Spectra Table", DT::dataTableOutput("fileList_sample")
                           )
                         )
                       )
              ),
              tabPanel(value = "figure_tab","Figure Display",
                       br(),
                       br(),
                       fluidRow(
                         column(width = 1,
                                br(),
                                actionButton("export_figures", "Export Figures")
                         ),
                         column(3, 
                                textInput(inputId = "export_header", "Figure File Prefix")
                         )
                       ),
                       mainPanel(
                         plotOutput("grid.plot",width = "150%",height = "700px")
                       )
              ),
              tabPanel(value = "inspect_tab","Inspect Spectra Display",
                       br(),
                       br(),
                       fluidRow(
                         column(width = 1,
                                actionButton("next_spectrum", "Next Spectrum")
                         ),
                         column(width = 1,
                                actionButton("back_spectrum", "Previous Spectrum")
                         )
                       ),
                       
                       br(),
                       br(),
                       fluidRow(
                         column(width = 1,
                                br(),
                                actionButton("find_spectrum", "Find Spectrum")
                         ),
                         column(3, 
                                textInput(inputId = "specific_spectra", "Partial Filename")
                         )
                       ),
                       mainPanel(
                         plotOutput("inspect_plot",width = "150%",height = "700px")
                       )
              )
  )
)



server <- function(input, output, session) {
  
  react.var<-reactiveValues(spectra_reference_tbl=NULL,
                            spectra_sample_tbl=NULL,
                            inspect_spectra=NULL,
                            plot_list=NULL,
                            start=1)
  
  #Generate data tables for mapping sample and refernece spectra  
  observeEvent(input$load_files,{
    if (is.na(input$dir[[1]][2])){
      showNotification("You Need To Select a Directory")
    } else {
      if (input$sample_pattern %in% ""){
        sample.pattern=NULL
      } else{
        sample.pattern=input$sample_pattern
        
        
        files.sample<-list.files(path=unlist(input$dir[[1]][2]),
                                 pattern=sample.pattern)
        #calculate reference id vector
        n.samples<-length(files.sample);
        ref.id.vec<-sort(rep(seq(1,ceiling(n.samples/input$id_interval)),input$id_interval))[1:n.samples]
        react.var[["spectra_sample_tbl"]]<-data.frame(file.name=files.sample,
                                                      reference.id=ref.id.vec,
                                                      exclude=rep(0,n.samples))
        output$fileList_sample<-renderDT(react.var[["spectra_sample_tbl"]], selection = 'none', server = TRUE, editable='cell')
      }
    }
  })
  
  observeEvent(input$load_files,{
    if (!is.na(input$dir[[1]][2])){
      
      
      if (input$reference_pattern %in% ""){
        reference.pattern=NULL
      } else{
        reference.pattern=input$reference_pattern
        
        
        files.reference<-list.files(path=unlist(input$dir[[1]][2]),
                                    pattern=reference.pattern)
        react.var[["spectra_reference_tbl"]]<-data.frame(file.name=files.reference,
                                                         reference.id=seq(1,length(files.reference)),
                                                         exclude=rep(0,length(files.reference)))
        output$fileList_reference<-renderDT(react.var[["spectra_reference_tbl"]], selection = 'none', server = TRUE, editable='cell')
      }
    }
  })
  
  #Render outputs
  
  
  
  
  observeEvent(input$export_figures,{
    for (f in names(react.var[["plot_list"]])){
      switch(f,
             "spec.slope" = ggsave(plot=react.var[["plot_list"]]$f, paste(input$export_header,"spectral_slope_figure.pdf",sep="_"),device="pdf"),
             "withiness" = ggsave(plot=react.var[["plot_list"]]$f, paste(input$export_header,"withiness_figure.pdf",sep="_"),device="pdf"),
             "dendrogram" = ggsave(plot=react.var[["plot_list"]]$f, paste(input$export_header,"dendrogram_figure.pdf",sep="_"),dpi=300,device="pdf"),
             "integrate" = ggsave(plot=react.var[["plot_list"]]$f, paste(input$export_header,"integrate_figure.pdf",sep="_"),dpi=300,device="pdf")
      )
    }
  })
  
  observeEvent(input$data_type,{
    if (react.var[["start"]] == 0){
      if (input$data_type %in% "Raw Spectra (requires sample/reference spectra)"){
        updateTextAreaInput(session,"intensity_units",value = "Absorption")
        toggleState("intensity_units")
        toggleState("reference_pattern")
        toggleState("id_interval")
        toggleState("path_len")
      } else {
        toggleState("intensity_units")
        toggleState("id_interval")
        toggleState("reference_pattern")
        toggleState("path_len")
      }
    } else {
      toggleState("intensity_units")
      react.var[["start"]] <-0
    }
  })
  
  # Create absorbance spectra based on data tables  
  observeEvent(input$fileList_sample_cell_edit,{
    indx.change<-input$fileList_sample_cell_edit
    i <- indx.change$row
    j <- indx.change$col
    v<-indx.change$value
    
    react.var[["spectra_sample_tbl"]][i,j]<-coerceValue(v,react.var[["spectra_sample_tbl"]][i,j])
    
  })
  
  observeEvent(input$fileList_reference_cell_edit,{
    indx.change<-input$fileList_reference_cell_edit
    i <- indx.change$row
    j <- indx.change$col
    v<-indx.change$value
    
    react.var[["spectra_reference_tbl"]][i,j]<-coerceValue(v,react.var[["spectra_reference_tbl"]][i,j])
    
  })
  
  observeEvent(input$analyze_table,{
    if (is.na(input$dir[[1]][2])){
      showNotification("You Need To Select a Directory")
    } else if (input$spec.slope == TRUE & ((input$s1.upper > input$high.wavelength) | (input$s2.upper > input$high.wavelength) | (input$s1.lower < input$low.wavelength) | (input$s2.lower < input$low.wavelength))) {
      showNotification("One of the ranges in your spectral slope analysis\n extends outside the specified wavelength range.")
    }  else if (input$area.integrate == TRUE & ((input$min.integrate < input$low.wavelength) | (input$max.integrate > input$high.wavelength))) {
      showNotification("One of the ranges in your integration analysis\n extends outside the specified wavelength range.")
    } else if(length(react.var[["spectra_sample_tbl"]]) == 0){
      showNotification("You need to load in the directory.")
    }
    else {
      
      i<-1
      react.var[["plot_list"]] <-NULL
      withProgress(message = "Reading/Processing Spectra",value =  0, {
        if (input$data_type == "Raw Spectra (requires sample/reference spectra)"){
          files.sample<-react.var[["spectra_sample_tbl"]][,1]
          files.reference<-react.var[["spectra_reference_tbl"]][,1]
          
          d_all_samples<-data.frame(NULL)
          d_all_references<-data.frame(NULL)
          for (f in files.sample){
            
            f_tmp<-rollmean(read.table(paste(input$dir[[1]][2],f,sep="/")),k=input$smooth_window)
            colnames(f_tmp)<-c("wavelength.nm","sample.intensity")
            f.split<-strsplit(f,split = "_")
            name.tmp<-f#.split[[1]][1]
            reference.tmp<-react.var[["spectra_sample_tbl"]][i,2]
            exclude.tmp<-react.var[["spectra_sample_tbl"]][i,3]
            sample.tmp<-react.var[["spectra_sample_tbl"]][i,1]
            d_all_samples<-rbind(d_all_samples,data.frame(f_tmp,
                                                          sample.name=name.tmp,
                                                          reference.id=reference.tmp,
                                                          exclude=exclude.tmp))
            i<-i+1
          }
          
          i<-1
          for (f in files.reference){
            f_tmp<-rollmean(read.table(paste(input$dir[[1]][2],f,sep="/")),k=input$smooth_window)
            colnames(f_tmp)<-c("wavelength.nm","reference.intensity")
            f.split<-strsplit(f,split = "_")
            name.tmp<-f#.split[[1]][1]
            reference.tmp<-react.var[["spectra_reference_tbl"]][i,2]
            exlude.tmp<-react.var[["spectra_reference_tbl"]][i,3]
            d_all_references<-rbind(d_all_references,data.frame(f_tmp,
                                                                refernece.name=name.tmp,
                                                                reference.id=reference.tmp,
                                                                exclude=exclude.tmp))
            i<-i+1
          }
          
          d_all_references<-d_all_references %>% 
            filter(exclude==0) %>% 
            select(-exclude)
          d_all_samples<-d_all_samples %>% 
            filter(exclude==0) %>% 
            select(-exclude)
          
          d_merge<-merge(d_all_samples,d_all_references,by=c("reference.id","wavelength.nm"))
          
          d_spectra <- d_merge %>% 
            group_by(sample.name) %>%
            filter(wavelength.nm>=input$low.wavelength) %>%
            filter(wavelength.nm<=input$high.wavelength) %>%
            mutate(response=-log10(sample.intensity/reference.intensity)*2.303/input$path_len) %>%
            mutate(response.normalize=response/max(response)) %>%
            filter(!is.na(response))
        } else{
          files.sample<-react.var[["spectra_sample_tbl"]][,1]
          d_all_samples<-data.frame(NULL)
          for (f in files.sample){
            f_tmp<-rollmean(read.table(paste(input$dir[[1]][2],f,sep="/")),k=input$smooth_window)
            colnames(f_tmp)<-c("wavelength.nm","response")
            f.split<-strsplit(f,split = "_")
            name.tmp<-f#.split[[1]][1]
            # reference.tmp<-react.var[["spectra_sample_tbl"]][i,3]
            exclude.tmp<-react.var[["spectra_sample_tbl"]][i,3]
            sample.tmp<-react.var[["spectra_sample_tbl"]][i,1]
            d_all_samples<-rbind(d_all_samples,data.frame(f_tmp,
                                                          sample.name=name.tmp,
                                                          # reference.id=reference.tmp,
                                                          exclude=exclude.tmp))
            i<-i+1
          }
          d_all_samples<-d_all_samples %>% 
            filter(exclude==0) %>% 
            select(-exclude)
          
          d_spectra <- d_all_samples %>% 
            group_by(sample.name) %>%
            filter(wavelength.nm>=input$low.wavelength) %>%
            filter(wavelength.nm<=input$high.wavelength) %>%
            mutate(response.normalize=response/max(response))
        }
        incProgress(1)
      })
      
      if(input$write.file == TRUE){
        write.csv(d_spectra,file=paste(input$write.header,as.character(input$intensity_units),"_spectra.csv",sep="_"),quote = FALSE,row.names = FALSE)
      }
      
      if (input$inspect_spectra == TRUE){
        if (input$inspect_normalize %in% "Yes"){
          react.var[["inspect_spectra"]]$spectra_wide<-d_spectra %>% 
            group_by(sample.name) %>%
            select(sample.name,wavelength.nm,response.normalize) %>%
            spread(key=wavelength.nm,value=response.normalize)
        } else {
          react.var[["inspect_spectra"]]$spectra_wide<-d_spectra %>% 
            group_by(sample.name) %>%
            select(sample.name,wavelength.nm,response) %>%
            spread(key=wavelength.nm,value=response)
        }
        react.var[["inspect_spectra"]]$file.names<-as.character(unlist(react.var[["inspect_spectra"]]$spectra_wide[,1]))
        react.var[["inspect_spectra"]]$spectra_wide<-react.var[["inspect_spectra"]]$spectra_wide[,-1]
        react.var[["inspect_spectra"]]$wavelength<-as.numeric(colnames(react.var[["inspect_spectra"]]$spectra_wide))
        react.var[["inspect_spectra"]]$max.count<-nrow(react.var[["inspect_spectra"]]$spectra_wide)
        react.var[["inspect_spectra"]]$count<-1
        
        updateTabsetPanel(session,"main_tab",selected="inspect_tab")
        
        output$inspect_plot <- renderPlot({
          d_spectra_inspect_tmp <- data.frame(wavelength=react.var[["inspect_spectra"]]$wavelength,
                                              response=as.numeric(react.var[["inspect_spectra"]]$spectra_wide[react.var[["inspect_spectra"]]$count,]))
          if (input$inspect_normalize %in% "Yes"){
            y.label<- paste("Normalized",input$intensity_units, sep = " ")
          } else {
            y.label<- paste(input$intensity_units,"")
          }
          
          ggplot() +
            geom_line(data=d_spectra_inspect_tmp,aes(x=wavelength,y=response)) +
            theme_bw() +
            labs(x="Wavelength (nm)", y = y.label,title = react.var[["inspect_spectra"]]$file.names[react.var[["inspect_spectra"]]$count])
        })
      } 
      
      if (input$spec.slope == TRUE){
        
        d_spectral_s1 <- d_spectra %>% 
          group_by(sample.name) %>%
          filter(wavelength.nm>=input$s1.lower & wavelength.nm<=input$s1.upper) %>%
          select(response,wavelength.nm,sample.name) 
        colnames(d_spectral_s1)[c(1,2)]<-c("response.s1","wavelength.nm.s1")
        
        
        d_spectral_s2 <- d_spectra %>% 
          group_by(sample.name) %>%
          filter(wavelength.nm>=input$s2.lower & wavelength.nm<=input$s2.upper) %>%
          select(response,wavelength.nm,sample.name)
        colnames(d_spectral_s2)[c(1,2)]<-c("response.s2","wavelength.nm.s2")
        
        d_spectral.ratio <-
          merge(d_spectral_s1,d_spectral_s2,by=c("sample.name")) %>% 
          group_by(sample.name) %>% 
          nest() %>%
          mutate(slope.s2=map_dbl(data,function(df) coef(lm(log(response.s2)~wavelength.nm.s2,data=df))[2])) %>%
          mutate(slope.s1=map_dbl(data,function(df) coef(lm(log(response.s1)~wavelength.nm.s1,data=df))[2])) %>%
          mutate(SR=slope.s1/slope.s2)
        
        if (input$write.file == TRUE){
          write.csv(d_spectral.ratio[,c(2,4:6)],file=paste(input$write.header,"_spectral_ratio_analysis.csv",sep=""),quote = FALSE,row.names = FALSE)
        }
        
        react.var[["plot_list"]]$spec.slope <-ggplot(d_spectral.ratio,aes(x=sample.name,y=SR)) + 
          geom_point() + 
          theme_bw() +
          labs(x="Sample", y = "Spectral Ratio", title = "Spectral Slope") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      
      if (input$cluster == TRUE){
        if (input$cluster_norm %in% "Yes"){
          d_spectra_wide<-d_spectra %>% 
            select(response.normalize,wavelength.nm,sample.name) %>%
            group_by(sample.name) %>% 
            spread(key=wavelength.nm,value=response.normalize)
        } else {
          d_spectra_wide<-d_spectra %>% 
            select(response,sample.name,wavelength.nm) %>%
            group_by(sample.name) %>% 
            spread(key=wavelength.nm,value=response)
        }
        
        id_list<-d_spectra_wide[,1]
        d_spectra_dist<-d_spectra_wide[,-1]
        rownames(d_spectra_dist)<-as.character(unlist(id_list))
        dist_mat<-dist(d_spectra_dist,method="euclidean")
        
        
        if (nrow(id_list) > 10){
          k.max<-10
        } else {
          k.max<-nrow(id_list)
        }
        ss<-rep(0,k.max)
        o_clust<-hclust(dist_mat, method="ward.D")
        for(i in 1:k.max) {
          o_cut_clust<-cutree(o_clust, k=i)
          o_cluster_quality<-css(dist.obj=dist_mat, clusters=o_cut_clust)
          ss[i]<-o_cluster_quality$totwss
        }
        
      
        delta.ss<-(ss[1:k.max-1]/ss[1])-(ss[2:k.max]/ss[1])
        n.cluster<-min(which(delta.ss < input$cluster.threshold))
        p_dend<-dendro_data(o_clust,type="rectangle")
        cluster.vec<-merge(label(p_dend),data.frame(cluster=cutree(o_clust, k=n.cluster),label=unlist(id_list[1]),by="label"))
        if (input$write.file == TRUE){
          write.csv(cluster.vec[,c(5,4)],file=paste(input$write.header,"_cluster_analysis.csv",sep=""),quote = FALSE,row.names = FALSE)
        }
        # react.var[["plot_list"]]$cluster$cluster_ids<-cluster.vec
        react.var[["plot_list"]]$withiness<-ggplot(data=data.frame(withiness=ss,cluster=1:k.max),aes(x=cluster,y=withiness)) + 
          geom_point() +
          theme_bw() +
          labs(x = "Number of Clusters", y = "Withiness", title = "Withiness as a Function of Clusters")
        
        cluster.vec<-cluster.vec[order(cluster.vec$x),][,4]  
        react.var[["plot_list"]]$dendrogram <-ggplot() + 
          geom_segment(data=segment(p_dend), aes(x=x, y=y, xend=xend, yend=yend)) + 
          geom_text(data=label(p_dend), aes(x=x, y=y, label=label, hjust=0,color=as.factor(cluster.vec)), size=3) +
          coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + 
          labs(x="Distance",y="Sample",color="Cluster",title = "Spectra Dendrogram") +
          theme(axis.line.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.text.y=element_blank(),
                axis.title.y=element_blank(),
                panel.background=element_rect(fill="white"),
                panel.grid=element_blank())
        
      }
      
      if (input$area.integrate == TRUE){
        d_spectra_integrate <- d_spectra %>%
          group_by(sample.name) %>%
          filter(wavelength.nm>=input$min.integrate) %>%
          filter(wavelength.nm<=input$max.integrate) %>%
          summarize(area.integrate=sum(response))
        
        if (input$write.file == TRUE){
          write.csv(d_spectra_integrate[,c(2,3)],file=paste(input$write.header,"_",as.character(input$min.integrate),"_",as.character(input$max.integrate),"_integration_analysis.csv",sep=""),quote = FALSE,row.names = FALSE)
        }
        
        react.var[["plot_list"]]$integrate <-ggplot(d_spectra_integrate,aes(x=sample.name,y=area.integrate)) + 
          geom_point() +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(x="Sample", y = paste("Integrated ",input$intensity_units), title = "Integrated Spectra")
        
        
      }
      
      if (length(names(react.var[["plot_list"]])>0)){
        updateTabsetPanel(session,"main_tab",selected="figure_tab")
        output$grid.plot <- renderPlot({
          do.call("grid.arrange",c(react.var[["plot_list"]],ncol=2))
        })
      }
    }
  })
  
  observeEvent(input$next_spectrum, {
    
    if (length(react.var[["inspect_spectra"]])>0){
      if(react.var[["inspect_spectra"]]$count<react.var[["inspect_spectra"]]$max.count){
        react.var[["inspect_spectra"]]$count <<- react.var[["inspect_spectra"]]$count + 1 # Increment the counter by 1 when button is click
      }
      else{
        react.var[["inspect_spectra"]]$count <- 1 # Increment the counter by 1 when button is click
      }
      output$inspect_plot <- renderPlot({
        d_spectra_inspect_tmp <- data.frame(wavelength=react.var[["inspect_spectra"]]$wavelength,
                                            response=as.numeric(react.var[["inspect_spectra"]]$spectra_wide[react.var[["inspect_spectra"]]$count,]))
        
        if (input$inspect_normalize %in% "Yes"){
          y.label<- paste("Normalized",input$intensity_units, sep = " ")
        } else {
          y.label<- paste(input$intensity_units,"")
        }
        
        ggplot() +
          geom_line(data=d_spectra_inspect_tmp,aes(x=wavelength,y=response)) +
          theme_bw() +
          labs(x="Wavelength (nm)", y = y.label,title = react.var[["inspect_spectra"]]$file.names[react.var[["inspect_spectra"]]$count])
      })
    }
  })
  
  observeEvent(input$back_spectrum, {
    if (length(react.var[["inspect_spectra"]])>0){
      if(react.var[["inspect_spectra"]]$count>1){
        react.var[["inspect_spectra"]]$count <<- react.var[["inspect_spectra"]]$count - 1 # Increment the counter by 1 when button is click
      }
      else{
        react.var[["inspect_spectra"]]$count <- react.var[["inspect_spectra"]]$max.count # Increment the counter by 1 when button is click
      }
      output$inspect_plot <- renderPlot({
        d_spectra_inspect_tmp <- data.frame(wavelength=react.var[["inspect_spectra"]]$wavelength,
                                            response=as.numeric(react.var[["inspect_spectra"]]$spectra_wide[react.var[["inspect_spectra"]]$count,]))
        
        if (input$inspect_normalize %in% "Yes"){
          y.label<- paste("Normalized",input$intensity_units, sep = " ")
        } else {
          y.label<- paste(input$intensity_units,"")
        }
        
        ggplot() +
          geom_line(data=d_spectra_inspect_tmp,aes(x=wavelength,y=response)) +
          theme_bw() +
          labs(x="Wavelength (nm)", y = y.label,title = react.var[["inspect_spectra"]]$file.names[react.var[["inspect_spectra"]]$count])
      })
    }
  })
  
  observeEvent(input$find_spectrum, {
    if (length(react.var[["inspect_spectra"]])>0){
      
      file.name.match.indx<-grep(input$specific_spectra,react.var[["inspect_spectra"]]$file.names)
      if (length(file.name.match.indx) == 0){
        showNotification("There are no filenames matching this pattern.")
      } else {
        
        if (length(file.name.match.indx) > 1){
          showNotification("There are multiple filenames matching this pattern. The first match was used by default.")
          
        }  
        
        react.var[["inspect_spectra"]]$count <- file.name.match.indx[1]
        
        if (input$inspect_normalize %in% "Yes"){
          y.label<- paste("Normalized",input$intensity_units, sep = " ")
        } else {
          y.label<- paste(input$intensity_units,"")
        }
        
        output$inspect_plot <- renderPlot({
          d_spectra_inspect_tmp <- data.frame(wavelength=react.var[["inspect_spectra"]]$wavelength,
                                              response=as.numeric(react.var[["inspect_spectra"]]$spectra_wide[react.var[["inspect_spectra"]]$count,]))
          ggplot() +
            geom_line(data=d_spectra_inspect_tmp,aes(x=wavelength,y=response)) +
            theme_bw() +
            labs(x="Wavelength (nm)", y = y.label,title = react.var[["inspect_spectra"]]$file.names[react.var[["inspect_spectra"]]$count])
        })
      }
    }
  })
  
  #Select directory for analysis    
  dir_load<-shinyDirChoose(
    input,
    'dir',
    roots = c(wd = '.'),
    filetypes = c("tsv", "csv")
  )
  
  global <- reactiveValues(datapath = getwd())
  
  dir <- reactive(input$dir)
  
  output$dir <- renderText({
    global$datapath
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath(".")
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
               })
}

shinyApp(ui = ui, server = server)
