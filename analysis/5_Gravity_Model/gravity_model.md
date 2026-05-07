Gravity Model
================
Daniel Meyer
2026-05-01

``` r
#Load and dissolve NYC boundary
nyc_outline <- read_sf(here("data/raw_data/mapping_data/nynta2020_25d/nynta2020.shp")) %>%
  st_make_valid() %>%
  st_union() %>%
  st_transform(2263)
  #EPSG 2263 is NYC State Plane

nynta <- read_sf(here("data/raw_data/mapping_data/nynta2020_25d/nynta2020.shp"))
nynta_proj <- st_transform(nynta, crs = 4326)
```

``` r
# Load twitter data and filter by type to get exact coordinates
twitter_path <- here("data/raw_data/twitter_data/twitter_na_2017-09_ny.csv.gz")
sdf <- fread(twitter_path) %>%
  filter(type == "ll")
twitter_path <- here("data/raw_data/twitter_data/twitter_na_2017-10_ny.csv.gz")
odf <- fread(twitter_path) %>%
  filter(type == "ll")
twitter_path <- here("data/raw_data/twitter_data/twitter_na_2017-11_ny.csv.gz")
ndf <- fread(twitter_path) %>%
  filter(type == "ll") 
  # Type ll contains exact lat long while type p is more approximate

df <- rbindlist(list(sdf, odf, ndf), fill = TRUE)
```

``` r
# (This code was authored by James) #

# Create a lookup for unique home H3 cells
home_lookup <- df %>%
  distinct(home) %>%
  mutate(geometry = h3jsr::cell_to_point(home)) %>% 
  st_as_sf() %>% 
  st_transform(st_crs(nyc_outline))

# Determine if cells are inside NYC
home_lookup <- home_lookup %>%
  mutate(is_local = as.logical(st_intersects(geometry, nyc_outline, sparse = FALSE))) %>%
  st_drop_geometry()

# Merge results and label
df_final <- df %>%
  left_join(home_lookup, by = "home") %>%
  mutate(user_type = if_else(is_local, "local", "non-local"))

# Select only posts by local users
nycDataSept <- df_final %>% filter(is_local == T)
```

``` r
# Operating on n rows
sub_nyc_data <- slice_head(nycDataSept, n = 600000)

# Extracting necessary data and converting it into a simple feature
sf_nyc_data <- sub_nyc_data %>% 
  select(id, u_id, home, lon, lat) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
```

``` r
# Create table with number of posts by user
posts_table = sf_nyc_data$u_id |>
  table()
# Create weights column in sf object
#    This column stores the fraction of the user's posts that a single post represents:
#     ie if the user has 20 posts, the weight value would be .05
#    This normalization prevents users with many posts from drowning out the
#     data from users who post less frequently.

# Remove users with only 1 post so they don't
#  sway the gravity model 
sf_nyc_data = sf_nyc_data |>
  mutate(weight = 1/posts_table[as.character(u_id)]) |>
  filter(weight < 1)
```

``` r
# Join the dataframe to neighborhoods based on post location and user location
sf_nyc_data = sf_nyc_data |>
  st_join(nynta_proj, join = st_within) |>
  mutate(geometry_user = h3jsr::cell_to_point(home)) |>
  st_set_geometry("geometry_user") |>
  st_join(nynta_proj, join=st_within, suffix = c("_post", "_user")) |>
  select(c(id, u_id, home, weight,
           geometry, geometry_user,
           BoroName_post, NTAName_post, NTAType_post,
           BoroName_user, NTAName_user, NTAType_user)) |>
  st_set_geometry("geometry")

# Remove neighborhoods where the user's neighborhood is
#  documented as a cemetery or airport or park (clear data error)
sf_nyc_data = sf_nyc_data |>
  filter(
    NTAType_user != 7,
    NTAType_user != 8,
    NTAType_user != 9,
  )

# Calculate weighted post totals by neighborhood...
post_neighborhoods_norm = sf_nyc_data |>
  group_by(NTAName_post) |>
  summarize(sum = sum(weight), .groups = "drop") |>
  st_drop_geometry()
  # ...and store in a basic data frame

# Repeat for weighted user totals
colnames(post_neighborhoods_norm)[1] = "NTAName"
user_neighborhoods_norm = sf_nyc_data |>
  group_by(NTAName_user) |>
  summarize(sum = sum(weight), .groups = "drop") |>
  st_drop_geometry()
colnames(user_neighborhoods_norm)[1] = "NTAName"

# Join post and user totals to the neighborhood sf object
nynta_proj = nynta_proj |>
  left_join(post_neighborhoods_norm, 
            by="NTAName")
nynta_proj = nynta_proj |>
  left_join(user_neighborhoods_norm, 
            by="NTAName", suffix = c("_to", "_from"))
nynta_proj = nynta_proj |>
  mutate(sum_diff = sum_to - sum_from)
```

``` r
# Creating edge data frame
joint_neighborhoods_norm = sf_nyc_data |>
  group_by(NTAName_post, NTAName_user) |>
  summarize(sum = sum(weight), .groups = "drop") |>
  select(from = NTAName_user, to = NTAName_post, weight = sum) |>
  filter(
    
    !is.na(to),                         # Exclude edges to or from locations outside
                                        #    the neighborhoods
    weight > 0,                         # Minimum weighted count of posts for a
                                        #    a neighborhood to be considered
    from != "Tribeca-Civic Center",     # Exclude posts by users to and from a
    to != "Tribeca-Civic Center"        #    suspiciously over-represented neighborhood
    
    ) |>
  # The pair of neighborhoods below has an exceedingly and abnormally large volume of flow,
  #   but only in one direction, which would seem to suggest error in the data
  #   rather than an actual relationship; thus, they are being filtered out as well
  filter(
    from != "Bedford-Stuyvesant (East)" | to != "Bedford-Stuyvesant (West)"
  ) |>
  st_drop_geometry()

# Creating node data frame
nynta_points = nynta_proj |> st_centroid() |>
  select(NTAName)
```

    ## Warning: st_centroid assumes attributes are constant over geometries

``` r
nynta_points = nynta_points |>
  mutate(
    x = st_coordinates(nynta_points)[,"X"],
    y = st_coordinates(nynta_points)[,"Y"]
  ) |>
  st_drop_geometry()
```

``` r
# Adding the number of users with a home in the origin neighborhood of each edge
#  as a column in the edge data frame
joint_neighborhoods_norm = joint_neighborhoods_norm |>
  left_join(user_neighborhoods_norm, by = c("from" = "NTAName"))
names(joint_neighborhoods_norm)[names(joint_neighborhoods_norm) == "sum"] = "users_from"

# ...and repeated for the destination neighborhoods
joint_neighborhoods_norm = joint_neighborhoods_norm |>
  left_join(post_neighborhoods_norm, by = c("to" = "NTAName"))
names(joint_neighborhoods_norm)[names(joint_neighborhoods_norm) == "sum"] = "users_to"

# These are the population terms in the gravity model.
```

``` r
# Creating a matrix of approximate distances between neighborhoods
distance_matrix = nynta_proj |>
  st_centroid() |>
  st_distance()
```

    ## Warning: st_centroid assumes attributes are constant over geometries

``` r
# Creating a named list of neighborhood-index pairs to improve the time efficiency
#  of looking up a distance in the above matrix given neighborhood names
neighborhood_index_list = as.list(1:nrow(nynta_points)) |>
  setNames(nynta_points$NTAName)

# Create an empty distance column in the edge data frame...
joint_neighborhoods_norm = joint_neighborhoods_norm |>
  mutate(distance_m = 0)
# ...and fill it with the distances between the to and from nodes in each pair
for (n in 1:nrow(joint_neighborhoods_norm)) {
  joint_neighborhoods_norm[n,"distance_m"] = as.double(distance_matrix[
    neighborhood_index_list[[ as.character(joint_neighborhoods_norm[n,"from"]) ]],
    neighborhood_index_list[[ as.character(joint_neighborhoods_norm[n,"to"]) ]]
  ])
}

# Remove rows with no users in the destination node or where the user and post are in the same neighborhood
joint_neighborhoods_norm = joint_neighborhoods_norm |>
  filter(!is.na(users_to), to != from)
```

``` r
# A logarithm is applied to both sides of the gravity model so that
#  the exponent for the distance term can be calculated
#  as a coefficient.
ols = lm( log(weight) ~ log(users_from) + log(users_to) + log(distance_m),
        data = joint_neighborhoods_norm)
joint_neighborhoods_norm$weight = ols$residuals
 # The weight of a neighborhood is how much it exceeds (or falls short of)
 #  the OLS regression of the gravity model.
```

``` r
# Visualizing a graph of the n edges with the greatest deviation from the model
#  in either direction
n = 15
extreme_neighborhoods_norm = arrange(joint_neighborhoods_norm, weight)[1:n,] |>
  rbind( arrange(joint_neighborhoods_norm, -weight)[1:n,] )

graph <- tbl_graph(nodes = nynta_points, edges = extreme_neighborhoods_norm, directed = T)

# Plotting the graph
  ggraph(graph, x = x, y = y, layout = 'manual') +
  
  # Create a basemap from neighborhood polygons
  geom_sf(data = nynta_proj, inherit.aes = F, fill = "#feeded67", color = "#22222222") +
    
  # Establish aesthetics of edges
  geom_edge_link(aes(color = weight), width = .67, alpha = .67,
                 arrow = arrow(length = unit(1.5, 'mm'), type = "closed"))+
  scale_edge_color_distiller(
    palette = "RdBu", direction = -1,
    name = "Actual Flow Volume minus\nFlow Volume predicted by\nOLS Regression") +
  
  # Create labels
  labs(title = paste(
    "Neighborhoods with the",
    as.character(n), 
    "Highest\nand",
    as.character(n),
    "Lowest Residuals"
  )) +
  theme_void() +
  theme(
    legend.position = c(.21, .69),
    legend.title = element_text(size = 9, face = "italic"),
    plot.title = element_text(size = 12, face = "bold")
  ) 
```

![](gravity_model_files/figure-gfm/Graph_Visualization-1.png)<!-- -->
