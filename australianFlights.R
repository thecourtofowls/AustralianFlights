## ---- warning=FALSE, message=FALSE-----------------------------------------
library(tidyverse)
library(magrittr)


## ---- warning=FALSE,message=FALSE------------------------------------------
flights = readr::read_csv(
  "http://www.maths.usyd.edu.au/u/UG/JM/DATA1001/r/current/projects/2020data/flights.csv"
)
write_csv(flights, "flights.csv")
flights$Date <- as.Date(paste(flights$Year, flights$Month_num,"01", sep = "-"))


## ---- warning=FALSE, message=FALSE, fig.align='center', dpi=350, fig.width=10, fig.height=8----
##InOut-----------------------------
in_out_flights_data <- flights %>% 
  select(In_Out, All_Flights, Date) %>% 
  group_by(In_Out, Date) %>% 
  summarise(sum = sum(All_Flights))
#head(in_out_flights_data)
#Plot of Inbound and OutBound Flights Over Course of Time
plot_in_out_flight <- in_out_flights_data %>% 
  ggplot(aes(x = Date, y = sum, color = In_Out)) + 
  geom_line() + scale_x_date(date_breaks="1 year", date_labels="%Y") +
  geom_point() +
  theme_bw() + labs(title = "Total In-Bound and Out-Bound Flights") + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = "top",
        plot.title = element_text(hjust = 0.5)) + 
  scale_color_discrete(
    limits = c("I","O"),
      labels = c("Inbound", "OutBound")
  ) +
  guides(color = guide_legend(title = NULL))
ggsave("plot_in_out_flight.png",plot = plot_in_out_flight, 
       width = 25, height = 25, unit = "cm", 
       dpi = 400)
plot_in_out_flight



## ---- warning=FALSE, message=FALSE, fig.align='center', dpi=350, fig.width=10, fig.height=8----
## City Traffic ---------------------

in_out_city_flights_data <- flights %>% 
  select(In_Out, Australian_City, All_Flights, Date) %>% 
  group_by(In_Out, Australian_City, Date) %>% 
  summarise(sum = sum(All_Flights)) %>% 
  mutate(Bound = recode(In_Out, "I" = "In Bound", "O" = "Out Bound"))
#head(in_out_city_flights_data)
#Plot of Inbound and OutBound Flights Over Course of Time
plot_in_out_city_flight <- in_out_city_flights_data %>% 
  filter(Australian_City %in% c("Brisbane", "Melbourne", "Perth", "Sydney")) %>% 
  ggplot(aes(x = Date, y = sum, color = Australian_City)) + 
  geom_line() + scale_x_date(date_breaks="1 year", date_labels="%Y") +
  geom_point() +
  theme_bw() + labs(title = "Top Cities In Total In-Bound and Out-Bound Flights") + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold", size = rel(1.5))) + 
  scale_y_continuous(limits = c(0, max(in_out_city_flights_data$sum)))+
  guides(color = guide_legend(nrow=1,byrow=TRUE,title = NULL)) + 
  facet_grid(Bound ~.)
ggsave("plot_in_out_city_flight.png",plot = plot_in_out_city_flight, 
       width = 25, height = 25, unit = "cm", 
       dpi = 400)

plot_in_out_city_flight


## ---- warning=FALSE, message=FALSE, fig.align='center', dpi=350, fig.width=10, fig.height=8----
## Airline Traffic ---------------------
in_out_air_flights_data <- flights %>% 
  select(In_Out, Airline, All_Flights, Date) %>% 
  group_by(In_Out, Airline, Date) %>% 
  summarise(sum = sum(All_Flights)) %>% 
  mutate(Bound = recode(In_Out, "I" = "In Bound", "O" = "Out Bound"))
#head(in_out_air_flights_data)
#Plot of Inbound and OutBound Flights Over Course of Time
plot_in_out_air_flight <- in_out_air_flights_data %>% 
  filter(Airline %in% c(
    "Air New Zealand", "Emirates", "Jetstar", "Qantas Airways",
    "Singapore Airlines", "Virgin Australia","Cathay Pacific Airways")) %>% 
  #filter(sum>250) %>% 
  ggplot(aes(x = Date, y = sum, color = Airline)) + 
  geom_line() + scale_x_date(date_breaks="1 year", date_labels="%Y") +
  geom_point() +
  theme_bw() + labs(title = "Top Airlines With Total In and OutBound Flights") + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold", size = rel(1.5))) + 
  scale_y_continuous(limits = c(0, max(in_out_air_flights_data$sum)))+
  guides(color = guide_legend(nrow=1,byrow=TRUE,title = NULL)) + 
  facet_grid(Bound ~.)
ggsave("plot_in_out_air_flight.png",plot = plot_in_out_air_flight, 
       width = 25, height = 25, unit = "cm", 
       dpi = 400)

plot_in_out_air_flight


## ---- warning=FALSE, message=FALSE, fig.align='center', dpi=350, fig.width=10, fig.height=8----
## Route Traffic ---------------------
in_out_route_flights_data <- flights %>% 
  select(In_Out, Route, All_Flights, Date) %>% 
  group_by(In_Out, Route, Date) %>% 
  summarise(sum = sum(All_Flights)) %>% 
  mutate(Bound = recode(In_Out, "I" = "In Bound", "O" = "Out Bound"))
#head(in_out_route_flights_data)
#Plot of Inbound and OutBound Flights Over Course of Time
plot_in_out_route_flight <- in_out_route_flights_data %>% 
  filter(Route %in% c("AKL-SYD", "SYD-AKL")) %>% 
   #filter(sum>250) %>% 
  ggplot(aes(x = Date, y = sum, color = Route)) + 
  geom_line() + scale_x_date(date_breaks="1 year", date_labels="%Y") +
  geom_point() +
  theme_bw() + labs(title = "Top Airlines With Total In and OutBound Flights") + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold", size = rel(1.5))) + 
  guides(color = guide_legend(nrow=1,byrow=TRUE,title = NULL)) + 
  facet_grid(Bound ~.)
ggsave("plot_in_out_route_flight.png",plot = plot_in_out_route_flight, 
       width = 25, height = 25, unit = "cm", 
       dpi = 400)

plot_in_out_route_flight


