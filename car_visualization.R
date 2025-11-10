# Load required libraries
library(ggplot2)
library(gridExtra)

# Sample data
cars <- data.frame(
  car_id = 1:8,
  brand = c("Toyota", "Honda", "Ford", "BMW", "Mercedes", "Toyota", "Honda", "Ford"),
  model = c("Camry", "Civic", "Mustang", "X5", "C-Class", "Corolla", "Accord", "Focus"),
  year = c(2022, 2021, 2023, 2022, 2023, 2021, 2023, 2020),
  price = c(25000, 22000, 35000, 55000, 45000, 20000, 28000, 18000),
  is_available = c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)
)
customers <- data.frame(
  customer_id = 1:5,
  name = c("John Doe", "Jane Smith", "Robert Johnson", "Emily Davis", "Michael Wilson"),
  contact = c("john@email.com", "jane@email.com", "robert@email.com", "emily@email.com", "michael@email.com")
)
sales <- data.frame(
  car_id = c(1, 4, 6),
  customer_id = c(1, 2, 3)
)

# Plot 1: Brand Distribution
brand_counts <- as.data.frame(table(cars$brand))
p1 <- ggplot(brand_counts, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat="identity") +
  ggtitle("Car Distribution by Brand") +
  xlab("Brand") + ylab("Number of Cars") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 2: Price Distribution
p2 <- ggplot(cars, aes(x = price)) +
  geom_histogram(binwidth = 5000, fill="#6C5CE7", color="black", alpha=0.7) +
  ggtitle("Car Price Distribution") +
  xlab("Price ($)") + ylab("Number of Cars") +
  theme_minimal()

# Plot 3: Sales by Brand (Pie chart)
sold_cars <- cars[cars$car_id %in% sales$car_id, ]
sales_by_brand <- as.data.frame(table(sold_cars$brand))
p3 <- ggplot(sales_by_brand, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y") +
  ggtitle("Sales Distribution by Brand") +
  theme_minimal() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_blank())

# Plot 4: Availability Status
availability <- data.frame(
  status = c("Available", "Sold"),
  count = c(sum(cars$is_available), sum(!cars$is_available))
)
p4 <- ggplot(availability, aes(x = status, y = count, fill = status)) +
  geom_bar(stat="identity") +
  ggtitle("Car Availability Status") +
  ylab("Number of Cars") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 5: Year vs Price Scatter
p5 <- ggplot(cars, aes(x=year, y=price, color=brand)) +
  geom_point(size=4) +
  ggtitle("Car Year vs Price by Brand") +
  xlab("Year") + ylab("Price ($)") +
  theme_minimal()

# Arrange all plots on a single page
grid.arrange(p1, p2, p3, p4, p5, ncol=2)

# Optional: Save all the plots to a PDF file
# pdf("car_showroom_plots.pdf", width=14, height=10)
# grid.arrange(p1, p2, p3, p4, p5, ncol=2)
# dev.off()
