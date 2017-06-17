library(rPython)
library(magrittr)
library(readr)

sample <- read_csv("sample.csv") 
ids <- sample$root_id 
sample <- sample[217:449] 
rownames(sample) <- paste("id", ids, sep="_")
names <- colnames(sample) 
library(stringr) 
names <- str_replace_all(names, "countries.visited_", "") 
colnames(sample) <- names
sample <- as.matrix(sample)

python.exec("from couchsurfing import Api")
python.exec('api = Api("95schatt@gmail.com", "MIKKy1989")')
x = 2003965669
x = as.character(x)
python.assign("x", x)
b = python.method.call("api", "get_profile_by_id", x)

id = b$id
countries = b$about$countries$visited
df <- data.frame(matrix(unlist(countries), nrow=length(countries), byrow=T),stringsAsFactors=FALSE)
user_matrix <- matrix(1, 1, length(countries))
rownames(user_matrix) <- paste("id", id, sep = "_")
colnames(user_matrix) <- df$X2

full_user_matrix <- matrix(0, 1 , dim(sample)[2])
rownames(full_user_matrix) <- paste("id", id, sep = "_")
colnames(full_user_matrix) <- colnames(sample)

cols <- colnames(full_user_matrix)[colnames(full_user_matrix) %in% colnames(user_matrix)]
rows <- rownames(full_user_matrix)[rownames(full_user_matrix) %in% rownames(user_matrix)]
full_user_matrix[rows, cols] <- user_matrix[rows, cols]

sample = rbind(sample, full_user_matrix)


