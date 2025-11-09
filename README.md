# Project2 - Seoul Bike Sharing Data Explorer

The Seoul Bike Sharing Data Explorer is an interactive Shiny app where users can explore patterns in bike rentals across different weather and seasonal conditions.

This app uses data from the Seoul Bike Sharing Dataset.The dataset includes hourly rental counts for public bicycles in Seoul, South Korea, with corresponding weather and holiday information. The app features sidebar filters where users can select specific seasons and holiday types, and choose between temperature and rainfall each with an adjustable slider. It also contains three tabs that share information about the app and data, display a downloadable table, and display many kinds of data summaries. The data exploration tab allows users to generate one-way and two-way contingency tables, view numeric summaries, and explore various graphs. The graphs display with a loading spinner when not immediately rendered. By selecting specific criteria in the sidebar, a user can observe how seasons, holidays, temperature, and rainfall influence bike rentals in Seoul.

This app requires the following packages: "shiny", "ggplot2", "tidyverse", "DT", and "shinycssloaders". 
