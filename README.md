README
================

# City Flow: tracking movement through NYC using twitter data
Team Members: Ashe King, James McQuilkin, Chimaera Todd, Ben Vales, Daniel Meyer

## Repository Structure
```
├── 1_Data
│   ├── Raw_Data
│   │   ├── Mapping_Data        # Geographic boundary files, shapefiles
│   │   └── Twitter_Data        # Raw Twitter datasets (Not included in repository)
│   └── Derived_Data
│       ├── User_Data           # Cleaned, processed, and user-level datasets 
│       └── Nodes_and_edge_data # Csv files for node and edge data for the Network graph (Not included in repository)
├── 2_Analysis
│   ├── 2.1_Create_NYC_Basemaps        # Generates base maps of NYC
│   ├── 2.2_Data_Exploration           # Initial exploration and descriptive analysis
│   ├── 2.3_Create_Nodal_Graph         # Construction of node-based network graphs
│   ├── 2.4_Create_Neighborhood_Graph  # Neighborhood-level network analysis
│   ├── 2.5_Gravity_Model              # Modeling movement using gravity model
│   ├── 2.6_Animated_Heat_Flow_Map     # Visualization of flows
│
└── 3_Final_Outputs             # Final visualizations used in report/presentation
```

## Intro
  NYC has the most diversity of transit systems of any city in the United States containing subway lines, citi bikes, 
  taxis, ride shares, buses, cable cars, and ferries.The diversity of transportation allows for quick multi-modal transit
  across the city. At the same time, this makes tracing movement of people through NYC complex because of the inability to 
  properly track the movement of people. With a accurate map of movement through the NYC metro area transit planning and
  operation becomes easier. Knowing where people travel from and too can be an important part of filling transit gaps.

## Project Overview
  The goal of this project is to utilize web scrapped geo-tagged twitter data as a proxy for movement to create a network graph
  of movement through NYC. For the visualizations we are using twitter data from September of 2017. The twitter data was modified 
  to include a calculated/approximate home location of each user. The location of each user's home was approximated using this
  article as a basis https://doi.org/10.1080/13658816.2021.1887489. In addition we removed any data from users whose 'homes' were
  located outside of the political boundary of NYC. In order to obfuscate the personal data we have aggregated the user's post data and
  'home' data to a hexagonal cell using the H3 package at a resolution of 8 (Hex area ~3/4 km^2) It can be assumed that a tweet posted 
  outside of a user's 'home' is an area of the city that they in some way traveled to. In this way a map of a user's approximate travel 
  through the study area over the study period. Mapping the movement of every user over NYC creates a network map of travel through the city.
  
  ![Map of the Twitter data network visualization](https://github.com/AsheVKing/Geog-469-City-Flow/blob/main/analysis/3_Create_Nodal_Graph/Network_map.png)
  
  This is the output from the dataset that we used, but the code should be able to work if the csv/dataframe is formatted the same
  
## Data formatting

  Data utilized by this program should be formatted with the following structure
  
  id|u_id|created_at|home|lon|lat|type|text
  --|--|--|--|--|--|--|--
  twitter post id|user account id|date-time of post creation|approximte home location|longitude|latitude|type of media in post|conent of post
  


