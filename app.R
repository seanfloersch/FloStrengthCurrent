library(tidyverse)
library(stringr)
library(rsconnect)
library(shinythemes)
library(bslib)
library(shinydashboard)
library(htmlwidgets)
library(shinybrowser)
library(rvest)
options(DT.options = list(pageLength = 50, language = list(search = 'Filter:')))


NFLTeams <-read.csv("NFL/TeamMaster.csv")

MLBTeams <-read.csv("MLB/TeamMaster.csv")
MLBPlayers<-read.csv("MLB/Hitters.csv")%>%select(Name)
MLBPlayers1<-read.csv("MLB/Pitchers.csv")%>%select(Name)
MLBPlayers<-rbind(MLBPlayers,MLBPlayers1)

CFBTeam <-read.csv("CFB/TeamMaster.csv")

SoccerTeams<-read.csv("Soccer/TeamMaster.csv")
sp1<-read.csv("Soccer/GK.csv")%>%select(Player)
sp2<-read.csv("Soccer/Players.csv")%>%select(Player)
SoccerPlayers<-rbind(sp1,sp2)

###########################UI#######################################

ui<-fluidPage(theme=shinytheme("yeti"),
              navbarPage("FloStrength Current",id = "nav",
                         #####Home#####
                         tabPanel("Home",
                                  tabsetPanel(
                                    tabPanel("Power Rankings",
                                             fluidPage(
                                               fluidRow(
                                                 column(5),
                                                 column(2,selectizeInput("PowerRankSport",label=h3("Sport"),choices=c("MLB","NFL","Soccer"))),
                                                 column(5,)
                                               ),
                                               fluidRow(
                                                 column(1),
                                                 column(10,
                                                        DT::dataTableOutput("PowerRankings")),
                                                 column(1)
                                               )
                                             )),
                                    tabPanel("MVP Standings",
                                             fluidPage(
                                               fluidRow(
                                                 column(5),
                                                 column(2,selectizeInput("MVPRankSport",label=h3("Sport"),choices=c("MLB","NFL","Soccer"))),
                                                 column(5,)
                                               ),
                                               fluidRow(
                                                 column(1),
                                                 column(10,
                                                        DT::dataTableOutput("MVPRankings")),
                                                 column(1)
                                               )
                                             ))
                                    
                                  )
                         ),
                         #####MLB#####
                         tabPanel("MLB",
                                  tabsetPanel(
                                    tabPanel("Teams",
                                             fluidPage(
                                               fluidRow(
                                                 column(3,
                                                        radioButtons("MLBTeamStat", label = h3("Stat"),
                                                                     choices = list("Flo Stats"=1,"Hitting" = 2,"Pitching" = 3,"Standings"=4), 
                                                                     selected = 1)),
                                                 column(3,
                                                        selectizeInput("MLBTeamTeamFilt",label=h3("Team(s)"),choices=c(unique(MLBTeams$Team)),multiple=TRUE)),
                                                 column(3,
                                                        selectizeInput("MLBTeamDivisionFilt",label=h3("Division"),choices=c("All",unique(MLBTeams$Div)))),
                                                 column(3,
                                                        radioButtons("MLBTeamLeagueFilt", label = h3("League"),
                                                                     choices = list("Both"=1,"AL" = 2,"NL" = 3), 
                                                                     selected = 1))),
                                               fluidRow(
                                                 column(12,
                                                        DT::dataTableOutput("MLBTeam"))
                                               )
                                             )),
                                    tabPanel("Players",
                                             fluidPage(
                                               fluidRow(
                                                 column(3,
                                                        radioButtons("MLBPlayerType", label = h3("Player Type"),
                                                                     choices = list("All"=1,"Hitters" = 2,"Pitchers" = 3), 
                                                                     selected = 1)),
                                                 column(3,selectizeInput("MLBPlayerTeamFilt",label=h3("Team(s)"),choices=c(unique(MLBTeams$Team)),multiple=TRUE)),
                                                 column(3,
                                                        radioButtons("MLBPlayerStat", label = h3("Stat Type"),
                                                                     choices = list("FloStrength"=1,"Basic" = 2), 
                                                                     selected = 1)),
                                                 column(3,selectizeInput("MLBPlayerNameFilt",label=h3("Players(s)"),choices=c(unique(MLBPlayers$Name)),multiple=TRUE))
                                               ),
                                               fluidRow(
                                                 column(12,
                                                        DT::dataTableOutput("MLBPlayers")
                                                 )
                                               )
                                             ))
                                  )),
                         #####NFL#####
                         tabPanel("NFL",
                                  tabsetPanel(
                                    tabPanel("Teams",
                                             fluidPage(
                                               fluidRow(
                                                 column(3,
                                                        radioButtons("NFLTeamStat", label = h3("Stat"),
                                                                     choices = list("Flo Stats"=1,"Team" = 2,"Opponent" = 3,"Standings"=4), 
                                                                     selected = 1)),
                                                 column(3,
                                                        selectizeInput("NFLTeamTeamFilt",label=h3("Team(s)"),choices=c(unique(NFLTeams$Team)),multiple=TRUE)),
                                                 column(3,
                                                        selectizeInput("NFLTeamDivisionFilt",label=h3("Division"),choices=c("All",unique(NFLTeams$Division)))),
                                                 column(3,
                                                        radioButtons("NFLTeamLeagueFilt", label = h3("League"),
                                                                     choices = list("Both"=1,"NFC" = 2,"AFC" = 3), 
                                                                     selected = 1))),
                                               fluidRow(
                                                 column(12,
                                                        DT::dataTableOutput("NFLTeam"))
                                               )
                                             )),
                                    tabPanel("Players",
                                             fluidPage(
                                               fluidRow(
                                                 column(3,),
                                                 column(3,
                                                        radioButtons("NFLPlayerPos", label = h3("Position"),
                                                                     choices = list("All"=1,"QB" = 2,"RB" = 3,"WR"=4,"DEF"=5), 
                                                                     selected = 1)),
                                                 column(3,selectizeInput("NFLPlayerTeamFilt",label=h3("Team(s)"),choices=c(unique(NFLTeams$Team)),multiple=TRUE)),
                                                 column(3,)
                                               ),
                                               fluidRow(
                                                 column(12,
                                                        DT::dataTableOutput("NFLPlayers"))
                                               )
                                             ))
                                  )),
                         #####NCAAF#####
                         tabPanel("CFB",
                           fluidPage(
                             fluidRow(column(1,),
                                      column(2,
                                             radioButtons("CFBTeamStat", label = h3("Stat Type"),
                                                          choices = list("Flo Stats"=1,"Offense" = 2,"Defense" = 3,"Standings"=4), 
                                                          selected = 1)),
                                      column(3,
                                             selectizeInput("CFBTeamTeamFilt",label=h3("School(s)"),choices=c(unique(CFBTeam$Team)),multiple=TRUE)),
                                      column(3,
                                             selectizeInput("CFBTeamConfFilt",label=h3("Conference"),choices=c("All",unique(CFBTeam$Conference)))),
                                      column(2,
                                             radioButtons("CFBEZFilt", label = h3("EZ Filters"),
                                                          choices = list("None"=1,"Top 25" = 2,"Power 5" = 3), 
                                                          selected = 1)),
                                      column(1,)),
                             fluidRow(column(12,
                                             DT::dataTableOutput("CFBTeam")))
                           )
                         ),
                         #####Soccer#####
                         tabPanel("Soccer",
                           tabsetPanel(
                             tabPanel("Squads",
                                      fluidPage(
                                        fluidRow(
                                          column(2,),
                                          column(2,
                                                 radioButtons("SoccerTeamStat", label = h3("Stat Type"),
                                                              choices = list("Flo Stats"=1,"Team" = 2,"Opponent" = 3,"Standings"=4), 
                                                              selected = 1)),
                                          column(3,
                                                 selectizeInput("SoccerTeamTeamFilt",label=h3("Squad(s)"),choices=c(unique(SoccerTeams$Squad)),multiple=TRUE)),
                                          column(3,
                                                 selectizeInput("SoccerTeamLeagueFilt",label=h3("League"),choices=c("All",unique(SoccerTeams$League)))),
                                          column(2,)),
                                        fluidRow(
                                          column(12,
                                                 DT::dataTableOutput("SoccerTeam"))
                                        )
                                      )),
                             tabPanel("Players",
                                      fluidPage(
                                        fluidRow(
                                          column(2,
                                                 selectizeInput("SoccerPlayerPosFilt",label=h3("Position"),choices=c("All","GK","DF","MF","FW"))),
                                          column(2,
                                                 radioButtons("SoccerPlayerStat", label = h3("Stat Type"),
                                                              choices = list("Flo Stats"=1,"Basic" = 2), 
                                                              selected = 1)),
                                          column(3,
                                                 selectizeInput("SoccerPlayerTeamFilt",label=h3("Squad(s)"),choices=c("All",unique(SoccerTeams$Squad)))),
                                          column(3,
                                                 selectizeInput("SoccerPlayerLeagueFilt",label=h3("League"),choices=c("All",unique(SoccerTeams$League)))),
                                          column(2,
                                                 selectizeInput("SoccerPlayerName",label=h3("Player(s)"),choices=c(unique(SoccerPlayers$Player)),multiple=TRUE))),
                                        fluidRow(
                                          column(12,
                                                 DT::dataTableOutput("SoccerPlayers"))
                                        )
                                      ))
                           )
                         )
                         #############
                         )
              
              )
###########################SERVER#######################################
server <- function(input, output) {
  ######Home######
  output$PowerRankings <- DT::renderDataTable({
    if(input$PowerRankSport=="MLB"){
      df<-read.csv("MLB/TeamMaster.csv")%>%
        select(Team,FloStrength,W,L,"Wpct"=WinPercent)%>%
        arrange(-FloStrength)%>%
        slice(1:10)
    }
    if(input$PowerRankSport=="NFL"){
      df <-read.csv("NFL/TeamMaster.csv")%>%
        mutate(FloStrength=round(.2*FloPlayer+.5*FloTeam+.3*FloSched,3))%>%
        arrange(-FloStrength)%>%
        slice(1:10)%>%
        select(Team,FloStrength,"W"=Wins,"L"="Losses")%>%
        mutate(Wpct= round((W)/(W+L),3))
    }
    if(input$PowerRankSport=="Soccer"){
      df <-read.csv("Soccer/TeamMaster.csv")%>%
        arrange(-FloStrength)%>%
        select(Squad,League,FloStrength,W,L,D,Pts)%>%
        slice(1:10)
    }
    DT::datatable(df, options = list(dom = 't'))
  })
  output$MVPRankings <- DT::renderDataTable({
    if(input$MVPRankSport=="MLB"){
      df<-read.csv("MLB/Hitters.csv")%>%
        select(Name,Team,Pos,"FloStrength"="OffFS",FloValue)
      df1<-read.csv("MLB/Pitchers.csv")%>%
        select(Name,Team,Pos,"FloStrength",FloValue)
      df<-rbind(df,df1)%>%
        group_by(Name,Team)%>%
        mutate(FloValue=round(sum(FloValue),2))%>%
        mutate(FloStrength=round(mean(FloStrength),2))%>%
        slice(1)%>%
        ungroup()%>%
        arrange(-FloValue)%>%
        slice(1:10)
    }
    if(input$MVPRankSport=="NFL"){
      df1<-read.csv("NFL/qb.csv")%>%
        select(Name,Age,Tm,Pos,FloStrength,Value)%>%
        mutate(Value = round(Value,2))%>%
        mutate(FloStrength = round(FloStrength,2))
      df2<-read.csv("NFL/rb.csv")%>%
        select(Name,Age,Tm,Pos,FloStrength,Value)%>%
        mutate(Value = round(Value,2))%>%
        mutate(FloStrength = round(FloStrength,2))
      df3<-read.csv("NFL/wr.csv")%>%
        select(Name,Age,Tm,Pos,FloStrength,Value)%>%
        mutate(Value = round(Value,2))%>%
        mutate(FloStrength = round(FloStrength,2))
      df4<-read.csv("NFL/def.csv")%>%
        select(Name,Age,Tm,Pos,"FloStrength"="DefFS",Value)%>%
        mutate(Value = round(Value,2))%>%
        mutate(FloStrength = round(FloStrength,2))
      df<-rbind(df1,df2,df3,df4)%>%
        group_by(Name,Pos,Age)%>%
        mutate(Value=round(sum(Value),2))%>%
        slice(1)%>%
        ungroup()%>%
        arrange(-Value)%>%
        slice(1:10)%>%
        select(-Age)
    }
    if(input$MVPRankSport=="Soccer"){
      df1 <-read.csv("Soccer/Players.csv")%>%
        select(Player,Squad, League,Pos, MP, FloStrength,FloValue)
      df2<-read.csv("Soccer/GK.csv")%>%
        select(Player,Squad, "League",Pos, MP, "FloStrength"=Goalkeeping,FloValue)
      df<-rbind(df1,df2)%>%
        arrange(-FloValue)%>%
        mutate(FloValue=round(FloValue,2))%>%
        mutate(FloStrength=round(FloStrength,2))%>%
        slice(1:10)%>%
        select(Player,Squad,League,Pos,FloStrength,FloValue)
    }
    DT::datatable(df, options = list(dom = 't'))
  })
  ######NFL######
  output$NFLTeam <- DT::renderDataTable({
    df <-read.csv("NFL/TeamMaster.csv")%>%
      mutate(FloStrength=round(.2*FloPlayer+.5*FloTeam+.3*FloSched,3))%>%
      arrange(-FloStrength)
    if(length(input$NFLTeamTeamFilt)!= 0){
      df<-df%>%
        filter(Team %in% input$NFLTeamTeamFilt)
    }
    if(input$NFLTeamDivisionFilt != "All"){
      df<-df%>%
        filter(Division == input$NFLTeamDivisionFilt)
    }
    if(input$NFLTeamLeagueFilt== 2){
      df<-df%>%
        filter(League == "NFC")
    }
    if(input$NFLTeamLeagueFilt== 3){
      df<-df%>%
        filter(League == "AFC")
    }
    if(input$NFLTeamStat==1){
      df<-df[,c(1,48,26:43)]%>%
        select(-Passing,-Rushing,-OppPassing,-OppRushing,-yearID)
    }
    if(input$NFLTeamStat==2){
      df<-df[,c(1,48,2:11,21,23)]%>%
        rename("Points"="OffPoints")
    }
    if(input$NFLTeamStat==3){
      df<-df[,c(1,48,2,12:20,22,24)]
    }
    if(input$NFLTeamStat==4){
      df<-df[,c(1,2,48,44,45,21,22,46,47)]%>%
        mutate(Ties=GP-Wins-Losses)%>%
        mutate(Wpct = round((Wins+.5*Ties)/GP,3))%>%
        arrange(-Wpct)%>%
        mutate(PD = OffPoints-OppPoints)%>%
        select(Team,GP,"W"=Wins,"L"=Losses,"T"=Ties,Wpct,FloStrength, "PS"="OffPoints","PA"="OppPoints","PD","Div"=Division,"Lg"="League")
    }
    DT::datatable(df,options = list(scrollX = TRUE))
    })
  output$NFLPlayers<-DT::renderDataTable({
    if(input$NFLPlayerPos==1){
      df1<-read.csv("NFL/qb.csv")%>%
        select(Name,Age,Tm,Pos,FloStrength,Value)%>%
        mutate(Value = round(Value,2))%>%
        mutate(FloStrength = round(FloStrength,2))
      df2<-read.csv("NFL/rb.csv")%>%
        select(Name,Age,Tm,Pos,FloStrength,Value)%>%
        mutate(Value = round(Value,2))%>%
        mutate(FloStrength = round(FloStrength,2))
      df3<-read.csv("NFL/wr.csv")%>%
        select(Name,Age,Tm,Pos,FloStrength,Value)%>%
        mutate(Value = round(Value,2))%>%
        mutate(FloStrength = round(FloStrength,2))
      df4<-read.csv("NFL/def.csv")%>%
        select(Name,Age,Tm,Pos,"FloStrength"="DefFS",Value)%>%
        mutate(Value = round(Value,2))%>%
        mutate(FloStrength = round(FloStrength,2))
      df<-rbind(df1,df2,df3,df4)%>%
        group_by(Name,Pos,Age)%>%
        mutate(Value=round(sum(Value),2))%>%
        slice(1)%>%
        ungroup()%>%
        arrange(-Value)
    }
    if(input$NFLPlayerPos==2){
      df<-read.csv("NFL/qb.csv")[,c(1:4,16,17,6,7,10:15)]%>%
        arrange(-Value)%>%
        mutate(Value = round(Value,2))%>%
        mutate(FloStrength = round(FloStrength,2))
    }
    if(input$NFLPlayerPos==3){
      df<-read.csv("NFL/rb.csv")[,c(1:3,5,13,14,6:12)]%>%
        arrange(-Value)%>%
        mutate(Value = round(Value,2))%>%
        mutate(FloStrength = round(FloStrength,2))
    }
    if(input$NFLPlayerPos==4){
      df<-read.csv("NFL/wr.csv")[,c(1:3,5,13,14,6:12)]%>%
        arrange(-Value)%>%
        mutate(Value = round(Value,2))%>%
        mutate(FloStrength = round(FloStrength,2))
    }
    if(input$NFLPlayerPos==5){
      df<-read.csv("NFL/def.csv")[,c(1:4,16:20,6:15)]%>%
        arrange(-Value)%>%
        rename("FloStrength"="DefFS")%>%
        mutate(Value = round(Value,2))%>%
        mutate(FloStrength = round(FloStrength,2))%>%
        mutate(Rush = round(Rush,2))%>%
        mutate(Coverage = round(Coverage,2))%>%
        mutate(Tackle = round(Tackle,2))
    }
    if(length(input$NFLPlayerTeamFilt)!= 0){
      df<-df%>%
        filter(Tm %in% input$NFLPlayerTeamFilt)
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  ######MLB######
  output$MLBTeam <- DT::renderDataTable({
    df <-read.csv("MLB/TeamMaster.csv")%>%
      arrange(-FloStrength)
    if(length(input$MLBTeamTeamFilt)!= 0){
      df<-df%>%
        filter(Team %in% input$MLBTeamTeamFilt)
    }
    if(input$MLBTeamDivisionFilt != "All"){
      df<-df%>%
        filter(Div == input$MLBTeamDivisionFilt)
    }
    if(input$MLBTeamLeagueFilt== 2){
      df<-df%>%
        filter(Lg == "AL")
    }
    if(input$MLBTeamLeagueFilt== 3){
      df<-df%>%
        filter(Lg == "NL")
    }
    if(input$MLBTeamStat==1){
      df<-df[,c(1,34,30:33,35:39)]
    }
    if(input$MLBTeamStat==2){
      df<-df[,c(1,34,15:28)]
    }
    if(input$MLBTeamStat==3){
      df<-df[,c(1,34,15,6:14)]
    }
    if(input$MLBTeamStat==4){
      df<-df[,c(1,34,2:5,15,29)]%>%
        rename("Wpct"="WinPercent")%>%
        arrange(-Wpct)
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$MLBPlayers <- DT::renderDataTable({
    if(input$MLBPlayerType=="1"){
      df<-read.csv("MLB/Hitters.csv")%>%
        select(Name,Team,Pos,"FloStrength"="OffFS",FloValue)
      df1<-read.csv("MLB/Pitchers.csv")%>%
        select(Name,Team,Pos,"FloStrength",FloValue)
      df<-rbind(df,df1)%>%
        group_by(Name,Team)%>%
        mutate(FloValue=round(sum(FloValue),2))%>%
        mutate(FloStrength=round(mean(FloStrength),2))%>%
        slice(1)%>%
        ungroup()%>%
        arrange(-FloValue)
    }
    if(input$MLBPlayerType=="2"){
      df<-read.csv("MLB/Hitters.csv")%>%
        arrange(-FloValue)%>%
        mutate(FloValue=round(FloValue,3))
      if(input$MLBPlayerStat==1){
        df<-df[,c(1,19,20,37,2,3,35,36,21,24)]%>%
          mutate(OffFS=round(OffFS,2))%>%
          mutate(DefFS=round(DefFS,2))%>%
          mutate(OffValue=round(OffValue,2))%>%
          mutate(FValue=round(FValue,2))
      }else{
        df<-df[,c(1,19,20,37,2,3,4:17,22)]%>%
          mutate(Fpct=round(Fpct,3))
      }
    }
    if(input$MLBPlayerType=="3"){
      df<-read.csv("MLB/Pitchers.csv")%>%
        arrange(-FloValue)%>%
        mutate(FloValue=round(FloValue,3))%>%
        mutate(FloStrength=round(FloStrength,3))
      if(input$MLBPlayerStat==1){
        df<-df[,c(1,2,22,25,3,4,10,24)]
      }else{
        df<-df[,c(1,2,22,25,3,4,10,24,20,19,6:9,11:15)]
      }
    }
    
    if(length(input$MLBPlayerTeamFilt)!= 0){
      df<-df%>%
        filter(Team %in% input$MLBPlayerTeamFilt)
    }
    if(length(input$MLBPlayerNameFilt)!= 0){
      df<-df%>%
        filter(Name %in% input$MLBPlayerNameFilt)
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  ######Soccer######
  output$SoccerTeam <- DT::renderDataTable({
    df <-read.csv("Soccer/TeamMaster.csv")%>%
      arrange(-FloStrength)
    if(input$SoccerTeamStat==1){
      df<-df[,c(1,35,3,37,12,14,15,28,30,32,13,33,38:41,36)]
    }
    if(input$SoccerTeamStat==2){
      df<-df[,c(1,35,3,37,4:11,12,14,15)]
    }
    if(input$SoccerTeamStat==3){
      df<-df[,c(1,35,3,37,19,21:28,30,32)]
    }
    if(input$SoccerTeamStat==4){
      df<-df[,c(1,35,3,37,16:18,20,31,4,19,15,32)]%>%
        arrange(-PtsPerc)
    }
    if(length(input$SoccerTeamTeamFilt)!=0){
      df<-df%>%
        filter(Squad %in% input$SoccerTeamTeamFilt)
    }
    if(input$SoccerTeamLeagueFilt!="All"){
      df<-df%>%
        filter(League == input$SoccerTeamLeagueFilt)
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  output$SoccerPlayers <- DT::renderDataTable({
    if(input$SoccerPlayerPosFilt=="All"){
      df1 <-read.csv("Soccer/Players.csv")%>%
        select(Player,Squad, League,Pos, MP, FloStrength,FloValue)
      df2<-read.csv("Soccer/GK.csv")%>%
        select(Player,Squad, "League",Pos, MP, "FloStrength"=Goalkeeping,FloValue)
      df<-rbind(df1,df2)%>%
        arrange(-FloValue)%>%
        mutate(FloValue=round(FloValue,2))%>%
        mutate(FloStrength=round(FloStrength,2))
    }
    if(input$SoccerPlayerPosFilt=="GK"){
      df<-read.csv("Soccer/GK.csv")%>%
        arrange(-FloValue)%>%
        mutate(FloValue=round(FloValue,2))%>%
        mutate(FloStrength=round(Goalkeeping,2))%>%
        select(Player, Pos, Squad,League, MP,Saves, "Shots"=SoTA,FloStrength,FloValue)
    }
    if(input$SoccerPlayerPosFilt%in%c("DF","MF","FW")){
      df<-read.csv("Soccer/Players.csv")%>%
        filter(Pos==input$SoccerPlayerPosFilt)%>%
        arrange(-FloValue)
      if(input$SoccerPlayerStat==1){
        df<-df[,c(1,4,5,6,7,14:19)]
      }else{
        df<-df[,c(1,4,5,6,7,9:13,18,19)]
        }
    }
    if(input$SoccerPlayerTeamFilt!="All"){
      df<-df%>%
        filter(Squad == input$SoccerPlayerTeamFilt)
    }
    if(input$SoccerPlayerLeagueFilt!="All"){
      df<-df%>%
        filter(League == input$SoccerPlayerLeagueFilt)
    }
    if (length(input$SoccerPlayerName)!=0){
      df<-df %>%
        filter(Player %in% input$SoccerPlayerName)
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
  ######CFB######
  output$CFBTeam <- DT::renderDataTable({
    df <-read.csv("CFB/TeamMaster.csv")%>%
      arrange(-FloStrength)
    for(i in c(27:34,40:44)){
      df[,i]=round(df[,i],3)
    }
    if(input$CFBEZFilt==2){
      df<-df[1:25,]
    }
    if(input$CFBEZFilt==3){
      df<-df%>%
        filter(Conference %in% c("Pac-12","Big 10","Big 12","ACC","SEC"))
    }
    if(input$CFBTeamStat==1){
      df<-df[,c(1,39,3,44,41:43,27:33)]
    }
    if(input$CFBTeamStat==2){
      df<-df[,c(1,39,44,3,27,28,32,37,2,16,20,17:19,12,10,11,6)]
    }
    if(input$CFBTeamStat==3){
      df<-df[,c(1,39,44,3,29,30,33,38,4,21,25,22:24,15,13,14,8)]
    }
    if(input$CFBTeamStat==4){
      df<-df[,c(1,39,44,3,35:38,40)]
    }
    if(length(input$CFBTeamTeamFilt)!= 0){
      df<-df%>%
        filter(Team %in% input$CFBTeamTeamFilt)
    }
    if(input$CFBTeamConfFilt!= "All"){
      df<-df%>%
        filter(Conference== input$CFBTeamConfFilt)
    }
    DT::datatable(df,options = list(scrollX = TRUE))
  })
}


shinyApp(ui, server)
