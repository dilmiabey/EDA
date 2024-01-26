library(ggplot2)
library(tidyverse)
library(dplyr)
library(factoextra)
library(forcats)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(plotly)
library(reshape2)
car = car_price_prediction

class(car$Engine.volume)
#####CONVERT Levy TO NUMERIC
car = car%>%mutate(Levy=as.numeric(Levy),Levy = ifelse(Levy == "-", NA, Levy))
head(car$Levy)

### HANDLING TURBO OF ENGINE VOLUME 
car =car %>%
  mutate(Engine.volume = as.numeric(gsub(" Turbo", "", as.character(Engine.volume))))
#  turbocharger = ifelse(grepl(" Turbo", as.character(Engine.volume)), "Yes", "No"))

### HANDLING KM OF MILEAGE
car = car%>%
  mutate(Mileage = as.numeric(gsub(" km", "", as.character(Mileage))))

####REMOVE ID
car = car %>% select(-ID)
#######REMOVE DOOR
car = car%>% select(-Doors)
########COMBINING CATEGORICAL TO OTHER
nlevels(as.factor(car$Fuel.type))
nlevels(as.factor(car$Color))
nlevels(as.factor(car$Manufacturer))
nlevels(as.factor(car$Model))

table_manufacturer = sort(table(car$Manufacturer),decreasing = TRUE)
manufacturer_percentages = prop.table(table_manufacturer) * 100
manufacturer_percentages[1:10]
car = car %>%
  mutate(Manufacturer = fct_lump(Manufacturer, prop = 0.05, other_level = "Other"))
nlevels(as.factor(car$Manufacturer))

table_fuel = sort(table(car$Fuel.type),decreasing = TRUE)#less than 1% basically renames pluh in hybrid to other
fuel_percentages = prop.table(table_fuel) * 100
head(fuel_percentages)
#car = car %>%
#mutate(Fuel.type = fct_lump(Fuel.type, prop = 0.01, other_level = "Other"))
nlevels(as.factor(car$Fuel.type))

table_model = sort(table(car$Model),decreasing = TRUE)
model_percentages = prop.table(table_model) * 100
head(model_percentages)

car = car %>%
  mutate(Model = fct_lump(Model, prop = 0.01, other_level = "Other"))
nlevels(as.factor(car$Model))

table_color = sort(table(car$Color),decreasing = TRUE)
color_percentages = prop.table(table_color) * 100
car = car %>%
  mutate(Model = fct_lump(Color, prop = 0.01, other_level = "Other"))
nlevels(as.factor(car$Model))

############ SPLIT TRAIN/TEST
train_indices = sample(1:nrow(car), 0.8 * nrow(car))
train = car[train_indices, ]
test = car[-train_indices, ]

######IMPUTATION ON LEVY (SINCE IT HAS OUTLIERS MEDIAN IMPUTATION)
train_levy_median =median(train$Levy, na.rm = TRUE)
train = train %>%mutate(Levy = ifelse(is.na(Levy), train_levy_median, Levy))
test_levy_median =median(test$Levy, na.rm = TRUE)
test = test %>%mutate(Levy = ifelse(is.na(Levy), test_levy_median, Levy))

####PRICE TO LN(PRICE)
train = train %>%
  mutate(ln_price = log(Price))

###HISTOGRAM
ggplot(train, aes(x = Price)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Price",
       x = "Price",
       y = "Frequency") +
  theme_minimal()

ggplot(train, aes(x = ln_price)) +
  geom_histogram(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histogram of ln(Price)",
       x = "ln(Price)",
       y = "Frequency") +
  theme_minimal()


#####BOXPLOTS
####PRICE by Category

train %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = factor(Category), y = ln_price, fill = Category)) +
  labs(title = 'Boxplot of log(price) by Category', x= 'Category',y='Log(Price)')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####PRICE by Fuel.type

train %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = factor(Fuel.type), y = ln_price, fill = Fuel.type)) +
  labs(title = 'Boxplot of log(price) by Fuel.type', x= 'Fuel.type',y='Log(Price)')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####PRICE by Leather.interior

train %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = factor(Leather.interior), y = ln_price, fill = Leather.interior)) +
  labs(title = 'Boxplot of log(price) by Leather.interior', x= 'Leather.interior',y='Log(price)')
##########

####kruskal wallis test
library(broom)
categorical_predictors <- c("Manufacturer", "Model", "Category", "Leather.interior",
                            "Fuel.type", "Gear.box.type", "Drive.wheels", "Wheel",
                            "Color")

# Selecting the categorical predictors from the 'train' dataset
categorical_data <- train %>% select(categorical_predictors)

# Creating an empty list to store Kruskal-Wallis test results
kt_results <- list()

# Loop through each categorical predictor
for (i in categorical_predictors) {
  # Using `[[i]]` to reference the actual column in the data frame
  kt_results[[i]] <- kruskal.test(ln_price ~ ., data = cbind(train[i], train['ln_price']))
}

# Creating an empty data frame to store variable names and p-values
pvalue_table <- data.frame(Variable = character(0), P_Value = numeric(0))

# Loop through each categorical predictor
for (i in categorical_predictors) {
  # Using `[[i]]` to reference the actual column in the data frame
  kt_result <- kruskal.test(ln_price ~ ., data = cbind(train[i], train['ln_price']))
  
  # Extracting p-value using broom::tidy()
  p_value <- tidy(kt_result)$p.value
  
  # Adding variable name and p-value to the data frame
  pvalue_table <- rbind(pvalue_table, data.frame(Variable = i, P_Value = p_value))
}

# Printing the resulting table
print(pvalue_table)



######ADVANCED ANALYSIS
####HEATMAP
numeric_var = c("Prod..year","Mileage","Engine.volume",'Airbags','Cylinders','ln_price')
data_corr = train %>% select(numeric_var)
corr_normalized = scale(data_corr)
corr_matrix_response = cor(corr_normalized)
corr_matrix_long = melt(corr_matrix_response)
ggplot(corr_matrix_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(Var1, Var2, label = round(value, 2)), vjust = 1) +
  labs(title = "Correlation Heatmap")



numeric_predictors = c("Prod..year","Mileage","Engine.volume",'Airbags','Cylinders')
numeric_data = train %>% select(numeric_predictors)
head(numeric_data)
data_normalized = scale(numeric_data)
head(data_normalized)
corr_matrix = cor(data_normalized)
#ggcorrplot(corr_matrix)
####  hEATMAP 



data_pca= princomp(corr_matrix)
summary(data_pca)

####score plot
scores =predict(data_pca, data_normalized)

# Create a dataframe with scores
score_df <- as.data.frame(scores[, 1:2])

# Create a score plot
library(ggplot2)
ggplot(score_df, aes(x = Comp.1, y = Comp.2)) +
  geom_point(color = "orange", size = 3) +
  labs(title = "Score Plot from PCA",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()
####

data_pca$loadings[, 1:2]
###SCREE PLOT 
fviz_eig(data_pca, addlabels = TRUE)

# BIPLOT
fviz_pca_var(data_pca, col.var = "black")


  ###############################################################

selected_predictors =train[, c("Mileage", "Levy","Engine.volume",
                               "Airbags","Cylinders","Prod..year")]
kdata = scale(selected_predictors)


fviz_nbclust(kdata, kmeans, method = "silhouette")

fviz_cluster(kmeans(kdata,centers = 2,iter.max = 100,nstart = 100), data = kdata)
###########################################











