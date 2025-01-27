# Import necessary packages
library(ggplot2)
library(ggthemes)
library(ggrepel) 
# Load the data
df <- read.csv(file='Economist_Assignment_Data.csv')
# Remove the index row created by csv file
df <- df[-1]
# Check the dataset
head(df)

# Creating scatter plot first
pl <- ggplot(df, aes(x=CPI, y=HDI, color=Region)) + geom_point(shape=1, size=3,stroke = 1.2) + scale_color_solarized()

### Add Trend line
# method = 'lm':'lm' stands for a linear model, fitting a linear regression line.
# formula = y ~ log(x):Fits a regression model where y is a function of log(x).
# se = F: Suppresses the shaded confidence interval around the trend line.
pl2 <- pl + geom_smooth(aes(group=1), method = 'lm', formula = y ~ log(x), se = F, color='red')

# Adding country labels
pl2 + geom_text(aes(label=Country))

# Subset is created to remove extra country names
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

# Using text Repel, connect the point and label
pl3 <- pl2 + geom_text_repel(
  aes(label = Country),
  color = 'gray20',
  data = subset(df, Country %in% pointsToLabel),
  check_overlap = T,          # Control overlap
  box.padding = 0.8,          # Padding between text and points
  point.padding = 0.8,        # Padding between points and labels
  segment.color = "gray50",   # Color of the connecting lines
  segment.size = 0.5          # Thickness of the connecting lines
)

pl4 <- pl3 + theme_bw() + scale_x_continuous(name = 'Corruption Perceptions Index, 2011(10=least corrupt)', limits = c(1, 10), breaks = 1:10)
# ':' creates a sequence of integers, use seq() for creating float point numbers
pl4 <- pl4 + scale_y_continuous(name='Human Development Index, 2011(1=Best)',limits = c(0.2, 1.0), breaks = seq(0.2, 1.0, 0.1))

# Theme to change style of axis title
pl4 <- pl4 + theme(axis.title.x = element_text(face = 'italic'), axis.title.y = element_text(face='italic'))
# Add title to the plot
pl4 <- pl4 + ggtitle(label="Corruption and Human development")
# Theme to style the title and remove grid lines
pl4 <- pl4 +
  theme(
    axis.title.x = element_text(face = 'italic'),
    axis.title.y = element_text(face = 'italic'),
    plot.title = element_text(face = 'bold'),  # Center title
    plot.title.position = 'plot',
    legend.position = "top",        # Move legend to the top
    legend.direction = "horizontal", # Make legend horizontal
    legend.box = "horizontal",      # Ensure single-row legend
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.border = element_blank(),      # Remove all panel borders
    axis.line.x = element_line(color = 'gray'), # Add bottom border
    axis.line.y = element_line(color = 'gray')  # Add left border
  ) +
  guides(color = guide_legend(nrow = 1))
# Add a customized horizontal grid to the plot
pl4 <- pl4+ geom_hline(yintercept = seq(0.2, 1.0, 0.1), color = "gray80")
print(pl4)
