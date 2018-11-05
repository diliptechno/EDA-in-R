number_of_deaths <- micr_map$obs
trace_1 <- micr_map$exp
x <- micr_map$state

data <- data.frame(x, number_of_deaths, trace_1)

p <- plot_ly(data, x = x, y = number_of_deaths, type = 'scatter', mode = 'lines') %>%
  add_trace(y = number_of_deaths, name = 'observed deaths', mode = 'lines+markers') %>%
  add_trace(y = trace_1, name = 'expected deaths', mode = 'lines+markers')+labs(x="state",
                                                                               y="number of deaths \n in thousands ",
                                                                               title="Comparison -observed deaths vs expected deaths")
ggplotly(p)

chart_link = plotly_POST(p, filename="line/mode1")
chart_link
##############################################################
setwd("C:/Users/User/Desktop/grad/stat/proj/idea/stat")
library(plotly)
df <- read.csv("2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "potentially excess deaths",
                           beef,'<br>', "deaths in non-metro region",dairy, 
                           '<br>',"deaths in metro region", wheat,
                            '<br>',
                           "deaths/100,000", total.fruits))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(df, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total.exports, text = ~hover, locations = ~code,
    color = ~total.exports, colors = 'Reds'
  ) %>%
  colorbar(title = "number of observed deaths") %>%
  layout(
    title = '2015 US Deaths by State <br> (for top 5 diseases)',
    geo = g
  )

ggplotly(p)

#########data for final


