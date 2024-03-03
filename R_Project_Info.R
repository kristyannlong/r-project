# For this project, you will analyze a data set from Kaggle (see link). 
# The Kaggle project is asking for us to do machine learning, but we're 
# just going to use our brains to draw conclusions from the data set. 
# This is a larger data set than previous examples, similar to the 
# Social Media Stocks data, so keep that in mind. Additionally, it is 
# very hard to quantify what a 'good' project looks like here, so I 
# will primarily be grading based on whether or not you have given the 
# assignment an honest attempt and produced analysis. I will be looking 
# for evidence of 'digging in' to the data, and a presentation of the 
# data. To that end, two files will be submitted: One, an R notebook 
# with all your inspection; please do not delete queries of the data 
# unless you have misspelled them. I want to see your work, even if 
# you ended up going in a different direction than you originally planned. 
# And then I would like to see a file that includes some presentation - 
# some graphs, maybe a paragraph-long writeup of what you discovered in 
# tandem with the graphs, or a short (3-5 minute) video of you presenting 
# your findings. Be creative! We've seen what data analysis looks like for 
# some time now; now it is your chance to turn and show your skills.


# Project -----------------------------------------------------------------


# The data set I chose is eCommerce Customer Service Satisfaction
# The dataset captures customer satisfaction scores for a one-month period at an e-commerce platform
# called Shopzilla (a pseudonym). It includes various features such as category and sub-category of
# interaction, customer remarks, survey response date, category, item price, agent details 
# (name, supervisor, manager), and CSAT score etc.

#  Analysis is at the end

{r}
install.packages("janitor")
install.packages("skimr")

library(lubridate)
library(readr)
library(janitor)
library(skimr)
library(dplyr)
library(tidyverse)


dat <- read_csv("C:\\Users\\krist\\OneDrive\\Data Analyst class work\\R\\R Project\\customer_support_data.csv")

# Pull the agent stats based on CSAT Score and create the column for time between contact

# original code that was not working.  
# agent_stats <- dat %>%
#   select(issue_responded) %>%
#   mutate(issue_responded = as.POSIXct(issue_responded, format = "%m/%d/%Y %H:%M")) %>%
#   select(`Issue_reported at`) %>%
#   mutate(issue_reported = as.POSIXct(`Issue_reported at`, format = "%m/%d/%Y %H:%M")) %>%
#   select(issue_reported, issue_responded) %>%
#   mutate(time_between_contact = issue_reported - issue_responded)
# group_by(Agent_name) %>%
#   summarize(
#     count = n(),
#     csat_avg = mean(`CSAT Score`, na.rm = TRUE),
#   ) %>%
#   filter(count > 10)
# view(agent_stats)

agent_stats <- agent_stats %>%
  group_by(Agent_name) %>%
  summarize(
    count = sum(!is.na(`CSAT Score`)),  # Count non-missing CSAT Scores
    csat_avg = mean(`CSAT Score`, na.rm = TRUE)
  ) %>%
  filter(count > 10) %>%
  left_join(dat, by = "Agent_name") %>%
  mutate(issue_reported = as.POSIXct(`Issue_reported at`, format = "%m/%d/%Y %H:%M"),
         issue_responded = as.POSIXct(issue_responded, format = "%m/%d/%Y %H:%M"),
         time_between_contact = issue_reported - issue_responded) %>%
  select(issue_reported, issue_responded, time_between_contact, Agent_name, `CSAT Score`, count, csat_avg)

view(agent_stats)

str(agent_stats)
summary(agent_stats)

# plot the agent_stats variables to ggplot


# # aggregate data by Agent_name
# agent_summary <- agent_stats %>%
#   group_by(Agent_name) %>%
#   summarize(
#     avg_CSAT_Score = mean(`CSAT Score`),
#     total_CSAT_Scores = sum(count)
#   )
# 
# # create plot
# ggplot(data = agent_summary, aes(x = avg_CSAT_Score, y = total_CSAT_Scores)) +
#   geom_point() +
#   labs(x = "Average CSAT Score", y = "Total CSAT Scores")

# Count the number of unique agents
num_agents <- agent_summary %>% 
  summarize(num_agents = n_distinct(Agent_name))

print(num_agents)

library(ggplot2)

# Aggregate data by Agent_name
agent_summary <- agent_stats %>%
  group_by(Agent_name) %>%
  summarize(
    avg_CSAT_Score = mean(`CSAT Score`),
    total_CSAT_Scores = sum(count)
  )

# Create bubble chart
ggplot(data = agent_summary, aes(x = avg_CSAT_Score, y = total_CSAT_Scores, size = total_CSAT_Scores, color = avg_CSAT_Score)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(5, 15)) +
  labs(x = "Average CSAT Score", y = "Total CSAT Scores", size = "Total CSAT Scores", color = "Average CSAT Score") +
  theme_minimal()



# Trying to see monthly information.  still not where i want it.  not sure how to get
# the monthly data for each agent broken down.

agent_stats$issue_reported <- as.Date(agent_stats$issue_reported)

agent_stats$month <- format(agent_stats$issue_reported, "%Y-%m")

monthly_summary <- agent_stats %>%
  group_by(month) %>%
  summarize(
    avg_CSAT_Score = mean(`CSAT Score`),
    total_CSAT_Scores = sum(count)
  )
view(monthly_summary)




# Just playing with Shiny to see how this would look.
ui <- fluidPage(
  titlePanel("CSAT Score Visualization"),
  sidebarLayout(
    sidebarPanel(
      # Sidebar content (if any)
    ),
    mainPanel(
      # Main panel content (plots, tables, etc.)
      plotOutput("csatPlot")
    )
  )
)

server <- function(input, output) {
  output$csatPlot <- renderPlot({
    ggplot(data = agent_summary, aes(x = avg_CSAT_Score, y = total_CSAT_Scores, size = total_CSAT_Scores, color = avg_CSAT_Score)) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(range = c(5, 15)) +
      labs(x = "Average CSAT Score", y = "Total CSAT Scores", size = "Total CSAT Scores", color = "Average CSAT Score") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)


# I found this to be a pretty tough assignment as the data was sparse on info.  I went with 
# charting the csat scores for the agents.  this was tough as there are over 1300 agents.
# After researching, I found that shiny is a pretty cool package that can give more
# control over the charts and can provide more info than what the regular charts can give.

# My analysis, based on the chart, is that this company is doing pretty well overall for 
# customer satisfaction.  There are a few low scores.  Reviewing the customer input can help
# to see where areas of opportunity are for the agents.  I had some issues creating the data
# customizations to see what I wanted to see.  I wanted to see each agent's csat scores
# monthly.  


#If I was to recommend something to the company, I would suggest they use data mining programs, such as CallMiner.  CallMiner allows the managers and agents to see transcriptions of their calls and provides a vast amount of # information based on how the company has it set up.  
# 

