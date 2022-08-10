#load packages
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)

view(yr2004)

# the 4 flags and the studyno's
disorders <- c("studyno", "anycd_ic", "emot_ic", "other_ic", "hyper_ic")

#filtering for only children with any psych diagnosis, and then show only columns for the 4 flags
comorbidities_table <- yr2004 %>%
  filter(any_ic=="PRESENT") %>%
  select(disorders)

view(comorbidities_table)


# first making lists containing the studyno's that had a disorder present for each of the 4 flags

BD <- as.list(filter(comorbidities_table %>% filter(anycd_ic=="Disorder present"))$studyno)
DA <- as.list(filter(comorbidities_table %>% filter(emot_ic=="Disorder present"))$studyno)
other <- as.list(filter(comorbidities_table %>% filter(other_ic=="Disorder present"))$studyno)
ADHD <- as.list(filter(comorbidities_table %>% filter(hyper_ic=="Disorder present"))$studyno)

# VENN DIAGRAM

install.packages("grid")
install.packages("futile.logger")
install.packages("VennDiagram")
library(grid)
library(futile.logger)
library(VennDiagram)


venn.plot <- draw.quad.venn(
  area1 = length(BD) ,
  area2 = length(DA),
  area3 = length(other),
  area4 = length(ADHD),
  n12 = comorbidities_table %>%
    filter(anycd_ic == "Disorder present") %>%
    filter(emot_ic == "Disorder present") %>%
    nrow(),
  n13 = 27,
  n14 = 32,
  n23 = 38,
  n24 = 32,
  n34 = 20,
  n123 = 18,
  n124 = 17,
  n134 = 11,
  n234 = 13,
  n1234 = 6,
  category = c("Behavioural \ndisorders", "Depression \nand Anxiety", "Other", "ADHD"),
  print.mode=c("raw","percent") , 
  fill = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"),
  cex = 0.9,
  cat.cex = 1,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.dist = c(0.235, 0.235, 0.1, 0.1), 
  cat.col = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"),
  lwd = rep(1, 4)
)


# CORRELATION PLOTS

# bar plots will show out of all the children that have a disorder, how common that disorder is

anydisorder_table <- yr2004 %>%
  filter(any_ic=="PRESENT") 

BD_plot <- ggplot(data = anydisorder_table, aes(x = any_ic, fill=anycd_ic)) +
  geom_bar() +
  labs(title = "Children with Behavioural Disorder diagnoses", x = "", y = "Count", fill="Diagnoses?") 
BD_plot

DA_plot <- ggplot(data = anydisorder_table, aes(x = any_ic, fill=emot_ic)) +
  geom_bar() +
  labs(title = "Children with Depression & Anxiety diagnoses", x = "", y = "Count", fill="Diagnoses?") 
DA_plot

other_plot <- ggplot(data = anydisorder_table, aes(x = any_ic, fill=other_ic)) +
  geom_bar() +
  labs(title = "Children with Other Psych diagnoses", x = "", y = "Count", fill="Diagnoses?") 
other_plot

ADHD_plot <- ggplot(data = anydisorder_table, aes(x = any_ic, fill=hyper_ic)) +
  geom_bar() +
  labs(title = "Children with ADHD  diagnoses", x = "", y = "Count", fill="Diagnoses?") 
ADHD_plot

install.packages("ggpubr")
library(ggpubr)

figure1 <- ggarrange(BD_plot, DA_plot, ADHD_plot, other_plot, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
figure1


# CLUSTERING

clustering_table <- as.integer(comorbidities_table)
clustering_table


studyno_2 <- as.integer(comorbidities_table$studyno)
anycd_ic_2 <- as.integer(comorbidities_table$anycd_ic) - 1
emot_ic_2 <- as.integer(comorbidities_table$emot_ic) - 1
other_ic_2 <- as.integer(comorbidities_table$other_ic) - 1
hyper_ic_2 <- as.integer(comorbidities_table$hyper_ic) - 1


clustering_table <- data.frame(anycd_ic_2, emot_ic_2, other_ic_2, hyper_ic_2)
rownames(clustering_table) <- studyno_2
view(clustering_table)

# practice clustering plots

install.packages("ggdendro")
install.packages("cluster")
library(ggplot2)
library(ggdendro)
library(cluster)

# hierarchical clustering using euclidian distance 
hc1 <- hclust(dist(clustering_table), "ave")  # hierarchical clustering
ggdendrogram(hc1, rotate = TRUE, size = 2, labels = FALSE)

# heirarchical clustering using gower metric 

d_dist<-daisy(clustering_table, metric = "gower")
hc2<-hclust(d_dist, method = "complete")
plot(hc2, labels=FALSE)
rect.hclust(hc, k=8, border="red")

clustering_table %>%
  mutate_all(as.character) %>%
  transmute(all = paste0(anycd_ic_2, emot_ic_2, hyper_ic_2, other_ic_2)) %>% 
  distinct() %>%
  arrange(all)

comorbidities_table %>%
  select(-studyno) %>%
  distinct()















