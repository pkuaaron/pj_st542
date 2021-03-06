library(ggplot2)

library(class)



shinyUI(
    navbarPage(
        "Shiny project for ST542 - Min Chen",
        tabPanel("Projects Introduction and Background",
                 mainPanel(
                     HTML(
                         "
    <h2 style='color:#009933'>Introduction </h2>
    <p>Expanded Food and Nutrition Education Program (EFNEP) is a program funded by  the United States Department of Agriculture (USDA) National Institute of Food and Agriculture to provide nutrition education to low-income families, particularly those with young children. In this study, the participants were educated food nutrition and taught how to conduct the food research. Before and after the classes, a survey with diet history, attitudes on food consumption and usage, and the demographic record was used for program evaluation.</p>
    <p>In the program, several classes integrated local food to engage their participants. We are trying to understand how the including of local food will impact the food behavior changes.</p>
                             
    <h2 style='color:#009933'>Background </h2>
    <p>Nutrition education is used to help participants to choose healthy food options so that their health can be improved, in order to maximize the effect of nutrition education, the USDA encouraged the researchers to incorporate persuasive message (e.g. in this research, local food were introduced) into their programs .  Some message is gain-framed, meaning they present beneficial outcomes, while loss-framed messages will lead to negative outcomes. In this research, the including of the local food may be considered as a gained-framed factor.</p>
    "
))),
        
        tabPanel("Introduction to Data",
                 mainPanel(
                     HTML(
                         "
<h3>Data</h3>
<p>This study used data collected from North Carolina, it has sample of forty-three counties. The participants was educated with a 6 or 12 weeks program with 6-18 people, and during the education program, some of them will be exposed to local food including introducing local food from local farmers or community gardens in food demonstrations and touring the class to farmers’ market.  For all the participants, a questionnaire that includes their diet history, attitudes on food consumption and usage, and the demographic record were collected at the beginning of and at the end of the program.
     </p>
     <p>
     In this study, 3554 low-income mothers participated in the program at the beginning, we lost follow up 790 of them during the process; and 254 participants didn’t complete the questionnaires, thus we got 2510 completed surveys, and 771 out of them were exposed to local food (figure 1).
     </p>
     
     <h3>Measures</h3>
     
     <p>Participant and household characteristics. Data on participant and household characteristics were collected: participants self-reported ethnicity, race, age, education, pregnancy & nursing status, children ages and house holdsize.
     </p>
     
     <p>
     Nutrition knowledge and behaviors. A validated behavior checklist to gauge nutrition knowledge and dietary behaviors was also collected at the beginning and end of the program. Answers to ten questions were collected (Table 1): plan meals ahead, compare prices, run out of food, use grocery list etc., and a scale of 1 to 5 was coded for answers as ‘never’, ‘seldom’, ‘sometimes’, ‘most of the time’, and ‘always’, respectively.
     </p>
                         "
                     )
                     )),
        
        
        tabPanel("Methodology",
                 mainPanel(
                     HTML(
                         "
     <p>Data will be analyzed using RStudio 1.0.44 (R 3.3.2). Firstly, I will run descriptive statistics on variables to summarize the distribution of demographic characteristics (e.g.: counties, genders, ages, highest_grade and race) for both the participants with and without local food exposure.
     </p>
     <p>
     Secondly, I will join the demographic data with the behavior data by the adult id, and combine the participants with local food exposure and participants without local food exposure (a new variable of Local_Food_Exp will be added to indicate whether they are exposed to local food or not). For each of the 10 questions (table 1) which are used to measure the EFNEP program, I will do the chi-squared test to determine whether the answers are significant different between different groups (e.g.: different age groups, different race groups, different gender groups, and different highest grade group), especially, I need to check whether there is significant difference between participants with local food exposure and without local food exposure. As the participant were asked the same questions before and after the program, I will do this kind of test for the answers before and after the program.</p>
     
     <p>Thirdly, in this study, our main purpose is to find whether local food will have positive impact on the behavior changes. For each of the 10 questions, I will calculate the notches of changes before and after the program for each participant (new variable Notches_Diff, e.g. if the answer before the program is ‘Never’, and after the program the answer for the same person is ‘Alway’, then the Notches_Diff will be 4). I will conduct another chi-squared test with variables Local_Food_Exp and Notches_Diff (will be treated as categorical variable, and proportion of each value will be derived).</p>
     
     <p>Lastly, as we may find there is significant difference regarding demographic characteristics between those participants with local food exposure and participants without local food exposure even before the program started, (that’s one of the reasons why I conduct chi-squared test in the 2nd step); even though we may be able to approve that the local food significantly impact the behavior changes, the impacts may come for those demographic differences, in order to exclude the impact of demographic characteristics, I will try to conduct the chi-squared test among those groups with no significant demographic differences.</p>
     
     <p>The chi-squared test is used to determine whether there is a significant difference regarding the observed frequency and the expected frequencies within one or more categories.</p>
     
     <p>Suppose that Variable A has r levels, and Variable B has c levels. The null hypothesis states that knowing the level of Variable A does not help you predict the level of Variable B. That is, the variables are independent . </p>
                         "
                     )
                     )),
        # Application title
        tabPanel(
            "Exploration of the EFNEP data",
            # Sidebar with options for the data set
            sidebarLayout(
                sidebarPanel(
                    h3("Select the Question:"),
                    selectizeInput(
                        "Question",
                        "Question",
                        selected = "open",
                        choices = levels(as.factor(c(paste('Q0',seq(9),sep=''),'Q10')))
                    ),
                    br(),
                    h3("Select the Gender:"),
                    selectizeInput(
                        "Gender",
                        "Gender",
                        selected = "all",
                        choices = c(levels(as.factor(EFNEP_data$Gender)), "all")
                    ),
                    br(),
                    checkboxInput("hideplot", "Hide the plot")
                ),
                
                # Show output
                mainPanel(
                    conditionalPanel(
                        condition = "input.hideplot == false",
                        downloadButton('downloadPlot', 'Download Plot'),
                        
                        plotOutput(
                            "EFNEP_dataPlot_local",
                            height = 300
                        )
                    ),
                    
                    conditionalPanel(
                        condition = "input.hideplot == false",
                        downloadButton('downloadPlot2', 'Download Plot2'),
                        
                        plotOutput(
                            "EFNEP_dataPlot_non_local",
                            height = 300
                        )
                    ),
                    downloadButton("downloadData", "Download"),
                    tableOutput("table")
                )
            )
        ),
        
        # Using ttest to analyze the data
        tabPanel(
            "Analysis with t-test",
            # Sidebar with options for the data set
            sidebarLayout(
                sidebarPanel(
                    h3("Select the Question:"),
                    selectizeInput(
                        "Question1",
                        "Question1",
                        selected = "open",
                        choices = levels(as.factor(c(paste('Q0',seq(9),sep=''),'Q10')))
                    )
                ),
                # Show output
                mainPanel(h3(textOutput('caption')))
            )
        ),
        
        # Using Chi-Squared to analyze the data
        tabPanel(
            "Analysis with Chi-Squared",
            # Sidebar with options for the data set
            sidebarLayout(
                sidebarPanel(
                    h3("Select the Question:"),
                    selectizeInput(
                        "Question1",
                        "Question1",
                        selected = "open",
                        choices = levels(as.factor(c(paste('Q0',seq(9),sep=''),'Q10')))
                    )
                ),
                # Show output
                mainPanel(h3(textOutput('caption')))
            )
        )
        
     )
 )
