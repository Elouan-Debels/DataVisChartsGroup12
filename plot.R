table = read.csv("C:/Users/eloua/Desktop/data vis/assignments/vis_python/prodtypes.csv")
table2 = read.csv("C:/Users/eloua/Desktop/data vis/assignments/vis_python/orders2.csv")

names(table2)
#area variable is missing, nog toevoegen

regions = read.csv("C:/Users/eloua/Desktop/data vis/assignments/vis_python/data/regions.csv")
names(regions)


#adding the area variable to the table
library(dplyr)

# Select only the relevant variables from the second table
regions_selected <- select(regions, Territory, Area)

# Perform a left join on 'Territory'
merged <- left_join(table2, regions_selected, by = "Territory")

# View the result
print(merged)


#now we create the variables we need for the plot.

#group over individual customers. We sum over the customers, creating the variables "value" and "amount of orders",
#which we need to create the groups. Also create the OrderDate_list, which contains all the order dates for an
#individual customer

# Group by CustomerID
result <- merged %>%
  group_by(CustomerID) %>%
  summarize(
    value = sum(CartPriceInCP),
    `amount of orders` = n(),
    `OrderDate_list` = list(OrderDate),
    AccountType = first(AccountType),
    Area = first(Area)
  )

# View the result
print(result)

#no missing values in the result table
missing_area <- sum(is.na(result$Area))

# Print the number of missing values
print(missing_area)


#now we need to bin the value and amount of orders variables.
#we create bins of 6, with equal size, that is the size of each bin is the same, but that does not mean the same 
#amount of customers will be in each bin

# Create bins for 'value'
result_bin_temp <- result %>%
  mutate(
    value_bin = cut(value, breaks = seq(0, max(value), length.out = 7), include.lowest = TRUE, labels = FALSE)
  )

# Create bins for 'amount of orders'
result_bin <- result_bin_temp %>%
  mutate(
    orders_bin = cut(`amount of orders`, breaks = seq(0, max(`amount of orders`), length.out = 7), include.lowest = TRUE, labels = FALSE)
  )

# View the result
print(result_bin)


#find the most prevalent groups of people
#first we group according to value_bin and orders_bin
result_grouped <- result_bin %>%
  mutate(
    group = paste(value_bin, orders_bin, sep = ",")
  )



# Count the occurrences of each group. This is to calculate the sizes of the groups
#group_counts <- result_grouped %>%
#  group_by(group) %>%
#  summarize(count = n()) %>%
#  arrange(desc(count))


# Count the occurrences of each group and create additional variables for region counts. for some reason it does not
#add up, but we will ignore that for now. So keep in mind to not use the count value to divide, but use the explicit
#sum of the different areas, so divide by (North_count + East_count+ West_count + South_count)
#group_counts <- result_grouped %>%
#  group_by(group) %>%
#  summarize(
#    count = n(),
#    North_count = sum(Area == "North"),
#    East_count = sum(Area == "East"),
#    West_count = sum(Area == "West"),
#    South_count = sum(Area == "South"),
#    Underdark_count = sum(Area == "Underdark"),
#    Key_count = sum(AccountType == "Key Account"),
#    No_Key_count = sum(AccountType == "No Key Account"),
#    Private_buyer_count = sum(AccountType == "Private Buyer")
#  ) %>%
#  arrange(desc(count))


# View the updated group_counts
#print(group_counts)


group_counts <- result_grouped %>%
  group_by(group) %>%
  summarize(
    count = n(),
    North_count = sum(Area == "North"),
    East_count = sum(Area == "East"),
    West_count = sum(Area == "West"),
    South_count = sum(Area == "South"),
    Underdark_count = sum(Area == "Underdark"),
    Key_count = sum(AccountType == "Key Account"),
    No_Key_count = sum(AccountType == "No Key Account"),
    Private_buyer_count = sum(AccountType == "Private Buyer"),
    OrderDate_list = list(OrderDate_list)  # Collect OrderDate_list for each group
  ) %>%
  arrange(desc(count)) #%>%
  #mutate(OrderDate_list = list(c(OrderDate_list)))  # Concatenate OrderDate_list vectors into a single list
#bovenstaande regel die u gecomment is was de oorzaak van alle problemen met betrekking tot de dates


# Check the resulting dataframe
print(group_counts)
str(group_counts)





# Arrange the instances by the count of occurrences in descending order
top_groups <- group_counts %>%
  arrange(desc(count)) %>%
  head(3)

# View the top three groups
print(top_groups)

#notice that the groups are very lopsided, the groups are not equally sized at all. In fact, of the possible 36
#groups, there are only 14 that actually occur, and of those 14 only 5 have a count above 50. This makes sense to
#some extent, since value and amount of items ordere will correlate, but the extent to which it occurs is extreme


#create the lists of dates for the different groups of customers. We need this to create the "orders over time" plot
#(which is just a line plot, or if possible a disk shape like in the original sketch)

# Group by the 'group' variable and combine the 'OrderDate list' for each group
merged_dates <- result_grouped %>%
  group_by(group) %>%
  summarize(OrderDate_list = list(unlist(OrderDate_list)))


#create the correct values for the horizontal and vertical axes
value_bins <- unique(result_bin$value_bin)
orders_bins <- unique(result_bin$orders_bin)

#> print(value_bins)
#[1] 1101314.4 550657.2  1651971.6 2202628.8 2753286   0        
#Levels: 0 550657.2 1101314.4 1651971.6 2202628.8 2753286
#> print(orders_bins)
#[1] 120 90  150
#Levels: 0 30 60 90 120 150


# Load required libraries
library(ggplot2)


# Extract x and y coordinates from the 'group' column
group_counts$value_bin <- as.integer(substring(group_counts$group, 1, 1))
group_counts$orders_bin <- as.integer(substring(group_counts$group, 3, 3))



sorted_counts <- group_counts[order(group_counts$count, decreasing = TRUE), ]

# Select the top three rows
top_three <- head(sorted_counts, 3)

#THIS IS THE SCATTTERPLOT USED IN THE FINAL COMBINED PLOT
# Create scatterplot with tooltips and updated reference dots, no zoomed-out axes
scatterplot_zoomedin <- ggplot(group_counts, aes(x = value_bin, y = orders_bin, size = count, label = count)) +
  geom_point(alpha = 0.5, color = "blue", shape = 1) +
  geom_text(size = 3, vjust = -0.5) +  # Add text labels to points
  labs(x = "value_binned", y = "orders_binned", title = "Scatterplot of groups, scaled according to size") +
  theme_minimal() +
  scale_size_continuous(range = c(1, 40), breaks = top_three$count, labels = top_three$count)

# Display the plot
print(scatterplot_zoomedin)

#make it so that the vertical axis has ticks with a jump of 1 between them
scatterplot_zoomedin <- scatterplot_zoomedin +
  scale_y_continuous(breaks = seq(0, max(group_counts$orders_bin), by = 1))  # Adjust y-axis ticks to increments of 1

# Print the plot
print(scatterplot_zoomedin)


# Create scatterplot with tooltips, updated reference dots, and zoomed-out axes
scatterplot <- ggplot(group_counts, aes(x = value_bin, y = orders_bin, size = count, label = count)) +
  geom_point(alpha = 0.5, color = "blue", shape = 1) + #using shape = 1 makes the circles look better. 16 also good
  geom_text(size = 3, vjust = -0.5) +  # Add text labels to points
  labs(x = "value_binned", y = "orders_binned", title = "Scatterplot of groups") +
  theme_minimal() +
  scale_size_continuous(range = c(1, 40), breaks = top_three$count, labels = top_three$count) +
  xlim(0, 6 + 1) +  # Adjust x-axis limits to show from 0 to 6 with extra space
  ylim(0, 6)        # Set y-axis limits to show from 0 to 6

# Display the plot
print(scatterplot)


#for combining the plots: perhaps zooming in again for the scatterplot is better, to save space.
library(gridExtra)
# Create a dummy pie chart (replace with your actual pie chart code)
dummy_pie_chart <- ggplot() + geom_bar()

# Arrange the scatterplot and dummy pie chart
combined_plot <- grid.arrange(scatterplot, dummy_pie_chart, nrow = 1)

# Display the combined plot
print(combined_plot)

#to do:
#pas de assen aan. nu toont het gewoon value_binned en orders_binned als cijfers 0-6, maar ik wil echte cijfers,
#echte waarden zoals die voorkomen in het origineel
#de andere plots:
#seasonality plot, line plot of als het kan op een schijf zoals origineel
#key-account plot, piechart bij voorkeur of iets anders
#area they are from.






#MISLUKTE POGING TOT AANPASSEN VAN DE ASSEN NAAR ORIGINELE CIJFERS

# Convert factors to character vectors for better axis labeling
value_bins <- as.character(value_bins)
orders_bins <- as.character(orders_bins)

scatterplot <- ggplot(group_counts, aes(x = factor(value_bin), y = factor(orders_bin), size = count, label = count)) +
  geom_point(alpha = 0.5, color = "blue", shape = 1) + #using shape = 1 makes the circles look better. 16 also good
  geom_text(size = 3, vjust = -0.5) +  # Add text labels to points
  labs(x = "value_binned", y = "orders_binned", title = "Scatterplot of groups") +
  theme_minimal() +
  scale_size_continuous(range = c(1, 40), breaks = top_three$count, labels = top_three$count) +
  scale_x_discrete(labels = value_bins, breaks = value_bins) +  # Set x-axis labels and breaks
  scale_y_discrete(labels = orders_bins, breaks = orders_bins) +  # Set y-axis labels and breaks
  xlim(0, 6 + 1) +  # Adjust x-axis limits to show from 0 to 6 with extra space
  ylim(0, 6)        # Set y-axis limits to show from 0 to 6

# Display the plot
print(scatterplot)



#pie charts

# Load necessary library
library(ggplot2)

# Subset the data to the first three rows
subset_data <- group_counts[1:3, ]

# Create pie charts for each row
for (i in 1:3) {
  row_data <- subset_data[i, ]
  row_data <- select(row_data, group, North_count, East_count, West_count, South_count, Underdark_count)
  row_data_long <- tidyr::pivot_longer(row_data, cols = -group, names_to = "Region", values_to = "Count")
  
  # Create pie chart
  pie_chart <- ggplot(row_data_long, aes(x = "", y = Count, fill = Region)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = paste("Pie Chart for Group:", row_data$group)) +
    theme_void()  # Remove axis and background
  
  # Print pie chart
  print(pie_chart)
}


# Load necessary libraries
library(ggplot2)

# Assuming subset_data and the for loop are already defined

# Create a list to store pie charts
pie_chart_list <- list()

# Create pie charts for each row and store them in the list
for (i in 1:3) {
  row_data <- subset_data[i, ]
  row_data <- select(row_data, group, North_count, East_count, West_count, South_count, Underdark_count)
  row_data_long <- tidyr::pivot_longer(row_data, cols = -group, names_to = "Region", values_to = "Count")
  
  # Create pie chart
  pie_chart <- ggplot(row_data_long, aes(x = "", y = Count, fill = Region)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = paste("Pie Chart for Group:", row_data$group)) +
    theme_void()  # Remove axis and background
  
  # Store pie chart in the list
  pie_chart_list[[i]] <- pie_chart
}

# Combine pie charts into one plot
combined_pie_chart <- cowplot::plot_grid(plotlist = pie_chart_list, ncol = 3)

# Display the combined plot
print(combined_pie_chart)






#THIS MONSTER OF A PIECE OF CODE BELOW WORKS, it creates the piecharts and combines the plot of the scatter with them

# Load necessary libraries
library(ggplot2)
library(cowplot)  # for plot_grid

# Assuming scatterplot is already defined

# Create a list to store pie charts
pie_chart_list <- list()

# Subset the data to the first three rows
subset_data <- group_counts[1:3, ]

# Create pie charts for each row and store them in the list
for (i in 1:3) {
  row_data <- subset_data[i, ]
  row_data <- select(row_data, group, North_count, East_count, West_count, South_count, Underdark_count)
  row_data_long <- tidyr::pivot_longer(row_data, cols = -group, names_to = "Region", values_to = "Count")
  
  # Create pie chart with labels for each part
  pie_chart <- ggplot(row_data_long, aes(x = "", y = Count, fill = Region, label = Count)) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(aes(label = Count), position = position_stack(vjust = 0.5), color = "white", size = 3) +  # Add text labels
    coord_polar("y", start = 0) +
    labs(title = paste("Distribution of customers over areas for Group", row_data$group)) +
    theme_void()  # Remove axis and background
  
  # Store pie chart in the list
  pie_chart_list[[i]] <- pie_chart
}

# Arrange pie charts horizontally
combined_pie_charts <- plot_grid(plotlist = pie_chart_list, nrow = 1)

# Combine scatterplot and pie charts vertically
combined_plot <- plot_grid(scatterplot_zoomedin, combined_pie_charts, ncol = 1, rel_heights = c(3, 1))

# Display the combined plot
print(combined_plot)



#ATTEMPT TO MAKE NESTED PIE CHARTS, TO DISPLAY KEY OWNERSHIP
#SHITTY PLOT MET LIJN OP, CAN BE USED FOR SOMETHING

# Load necessary libraries
library(ggplot2)
library(cowplot)  # for plot_grid

# Sample data
data <- data.frame(
  Region = c("A", "B", "C", "D", "E"),
  Count = c(30, 20, 10, 25, 15)
)

# Create pie chart
pie_chart <- ggplot(data, aes(x = "", y = Count, fill = Region, label = Count)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(Count/sum(Count) * 100), "%")), position = position_stack(vjust = 0.5), color = "white", size = 3) +  # Add text labels
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart") +
  theme_void()  # Remove axis and background

# Add distribution line
distribution_line <- geom_segment(
  aes(x = 0, y = 0, xend = cos(pi * 0.2) * 0.9, yend = sin(pi * 0.2) * 0.9),  # Adjust angle and length as needed
  color = "red",
  size = 3
)

# Combine pie chart and distribution line
combined_plot <- pie_chart + distribution_line

# Display the combined plot
print(combined_plot)


#NESTED PIE CHART. The inner pie chart is the key account no key accoutn or private buyer,
#displayed for each of the top three
#you would need to pivot that table, in the same way the table is pivoted for the sample. not going to do that now

# Load necessary library
library(ggplot2)


# Extract the first three rows
first_three_rows <- head(group_counts, 3)

# Create a new dataframe in the specified format
sample_data <- data.frame(
  group = rep(first_three_rows$group, each = ncol(first_three_rows) - 1),
  name = rep(names(first_three_rows)[-1], times = nrow(first_three_rows)),
  value = c(t(first_three_rows[, -1]))
)

# Display the new dataframe
print(sample_data)


# Create nested pie chart using ggplot
nested_pie_chart <- ggplot(sample_data, aes(x = factor(group), y = value, fill = factor(name))) +
  geom_col() +
  scale_x_discrete(limits = c("Group1", "Group2")) +
  coord_polar("y")

# Display the nested pie chart
print(nested_pie_chart)

names(group_counts)
str(group_counts)


sample_data <- data.frame(group= c('Group1', 'Group2',
                                   'Group1', 'Group2', 
                                   'Group1', 'Group2'), 
                          name= c('name1', 'name1',
                                  'name2', 'name2', 
                                  'name3', 'name3'),
                          value= c(25,30,35,20,40,50))
print(sample_data)




###########LINE PLOT

#unnest
library(dplyr)
library(tidyr)

#tibble that contains all the dates of orders. only the three rows we actually want are kept
#group_counts is alright. It's the group_counts_dates where something goes wrong, and all dates are put into each
#row

rows_to_keep <- c(2, 3, 6)
# Combine dates for each group
group_counts_dates <- group_counts %>%
  group_by(group) %>%
  summarize(combined_dates = toString(unlist(OrderDate_list))) %>%
  filter(row_number() %in% rows_to_keep)



#order date list bevat voor elke rij exact dezelfde data. er zijn 14 lijsten en die lijsten zijn uniek, maar het
#probleem is dat die 14 lijsten in elke rij zitten, dus elke rij bevat dezelfde waardes.

print(group_counts_dates)

#de data lijken niet te kloppen, voor elke rij dezelfde data. check of het dezelfde zijn, door gewoon elke rij te 
#tellen hoeveel er in zitten

#nu nog:
#order de data van klein naar hoog
#steek ze in bins misschien
#dan kan je plots maken ervoor. line plots of bar charts, kan beide. lineplot heeft de voorkeur. als barplot dan neem
#best bins in de vorm van maanden. bar is waarschijnlijk makkelijker om te doen, en makkelijker om te zien.

library(dplyr)
library(tidyr)
library(lubridate)


##########PLOT OVER TIME --IT WORKS

library(dplyr)
library(lubridate)


#SHIT WERKT. is wel drie keer hetzelfde plot. en het is gebind over maanden. en de plot zelf is niet zo duidelijk,
#dus in de data zelf gaat gezocht moeten worden naar de betekenis van die drops. maar eigenlijk is het plot min of
#meer stabiel, als je er echt naar kijkt, varieert enkel tussen 4200 - 4800

# Step 1: Split the combined_dates string into separate strings
group_counts_dates_split <- group_counts_dates %>%
  mutate(combined_dates = strsplit(combined_dates, ", "))

# Step 2: Convert the separate strings into dates
group_counts_dates_converted <- group_counts_dates_split %>%
  mutate(combined_dates = lapply(combined_dates, function(x) as.Date(x, format = "%d/%m/%Y")))

# Step 3: Order the dates
group_counts_dates_ordered <- group_counts_dates_converted %>%
  mutate(combined_dates = lapply(combined_dates, sort))

# Step 4: Bin the dates into groups of 7 days
group_counts_dates_bin <- group_counts_dates_ordered %>%
  mutate(bins = lapply(combined_dates, function(x) cut(x, breaks = "month", labels = FALSE)))

# Step 5: Count the size of each bin
#bin_counts <- unlist(lapply(group_counts_dates_bin$bins, table))
# Function to count occurrences of each bin value for a single row
# Custom function to count occurrences of each bin value for a single row
count_bin_values <- function(row) {
  table(row)
}

# Apply the function row-wise
bin_value_counts <- lapply(group_counts_dates_bin$bins, count_bin_values) #is a list of tables. each table can be 
#plotted then

# Custom function to plot counts of bin values for each row
plot_bin_value_counts <- function(bin_value_counts) {
  # Convert to data frame
  df <- data.frame(bin = as.numeric(names(bin_value_counts)), count = as.numeric(bin_value_counts))
  
  # Plot
  ggplot(df, aes(x = bin, y = count)) +
    geom_line() +
    geom_point() +
    labs(x = "Bin", y = "Count") +
    theme_minimal()
}

# Plot for each row
plots <- lapply(bin_value_counts, plot_bin_value_counts)

# Print the plots
plots


#wat nu nog doen:
#zorg ervoor dat het allemaal in 1 groot plot weergegeven kan worden.

#MULTIPLE SMALL PLOTS
library(gridExtra)

# Arrange plots in a grid
grid.arrange(grobs = plots, ncol = 1)

#IN ONE BIG PLOT
# Combine the data from the three plots
combined_data <- do.call(rbind, lapply(bin_value_counts, function(x) {
  data.frame(bin = as.numeric(names(x)), count = as.numeric(x))
}))

# Plot the combined data
timeplot = ggplot(combined_data, aes(x = bin, y = count, group = factor(rep(1:3, each = nrow(combined_data)/3)), color = factor(rep(1:3, each = nrow(combined_data)/3)))) +
  geom_line() +
  geom_point() +
  labs(x = "Bin", y = "Count", color = "Group") +
  theme_minimal()


# Plot the combined data with customized labels
timeplot <- ggplot(combined_data, aes(x = bin, y = count, group = factor(rep(1:3, each = nrow(combined_data)/3)), color = factor(rep(1:3, each = nrow(combined_data)/3)))) +
  geom_line() +
  geom_point() +
  labs(x = "Month", y = "Amount of Orders", color = "Group") +  # Adjust axis labels
  scale_color_discrete(labels = c("Group 2,4", "Group 2,5", "Group 3,5")) +  # Adjust legend labels
  theme_minimal()

# Print the plot
print(timeplot)




# Load the cowplot library
library(cowplot)

# Create combined plot with the new plot
combined_plot <- plot_grid(scatterplot_zoomedin, combined_pie_charts, timeplot, ncol = 1, rel_heights = c(2, 1, 1))
combined_plot



#ADDING THE annotations to the scatterplot, manually. now you can read the actual values, not just the bin number

# Calculate bin edges for 'value'
value_breaks <- seq(0, max(result$value), by = (max(result$value) - min(result$value))/7)

# Print the bin edges for 'value'
value_breaks

# Calculate bin edges for 'amount of orders'
orders_breaks <- seq(0, max(result$`amount of orders`), by = (max(result$`amount of orders`) - min(result$`amount of orders`))/7)

# Print the bin edges for 'amount of orders'
orders_breaks


#coordinates for the annotations for the horizontal axis
annotation_text_5 <- "1 644 359"
x_coord_5 <- 0.67
y_coord_5 <- 0.55

annotation_text_3 <- "986 615"
x_coord_3 <- 0.37
y_coord_3 <- 0.55

#annotations for the vertical axis
annotation_text_vert4 <- "41"
x_coord_vert4 <- 0.04
y_coord_vert4 <- 0.56

annotation_text_vert5 <- "51"
x_coord_vert5 <- 0.04
y_coord_vert5 <- 0.74

annotation_text_vert6 <- "62"
x_coord_vert6 <- 0.04
y_coord_vert6 <- 0.93


# Add annotation to the combined plot


combined_plot_test <- combined_plot +
  annotate("text", x = x_coord_5, y = y_coord_5, label = annotation_text_5, color = "red") +
  annotate("text", x = x_coord_3, y = y_coord_3, label = annotation_text_3, color = "red") +
  annotate("text", x = x_coord_vert4, y = y_coord_vert4, label = annotation_text_vert4, color = "blue") +
  annotate("text", x = x_coord_vert5, y = y_coord_vert5, label = annotation_text_vert5, color = "blue") +
  annotate("text", x = x_coord_vert6, y = y_coord_vert6, label = annotation_text_vert6, color = "blue")
  
  
# Print the updated combined plot
print(combined_plot_test)





#werkt. maar de annotaties zijn shit. dat is ez te fixen. als er dan nog tijd is, kan ik misschien nog andere dingen
#vinden.

#ook nog de annotaties van de originel scatterplots, die mogen ook nog aangepast worden
#de namen veranderen, dus niet groep 3,5 maar we "first group" ofzo => group 3,5 is eigenlijk wel logisch, dus laat maar
#de groepen daar rechts van de counts namen geven
#titel scatterplot: "scatterplot of groups, scaled by size"