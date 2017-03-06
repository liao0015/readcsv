#' LCA analysis
#'
#' read a lca csv file and extract information
#'
#' @export
#' @param data_merge data input

read_file <- function(data_merge){


#data_merge<-read.csv(filename)
#print(data_merge)

Kingdom_set<-subset(data_merge, select = c("Kingdom", "MS2.count"))
Phylum_set<-subset(data_merge, select = c("Phylum", "MS2.count"))
Class_set<-subset(data_merge, select = c("Class", "MS2.count"))
Order_set<-subset(data_merge, select = c("Order", "MS2.count"))
Family_set<-subset(data_merge, select = c("Family", "MS2.count"))
Genus_set<-subset(data_merge, select = c("Genus", "MS2.count"))
Species_set<-subset(data_merge, select = c("Species", "MS2.count"))

Kingdom_num<-aggregate(MS2.count~Kingdom, Kingdom_set, length)
names(Kingdom_num)[2]<-"num"
Kingdom_sum<-aggregate(MS2.count~Kingdom, Kingdom_set, sum)
names(Kingdom_sum)[2]<-"sum"
Kingdom_total<-merge(Kingdom_num, Kingdom_sum)
names(Kingdom_total)[1]<-"name"
Kingdom_total<-cbind(Kingdom_total, lca = "kingdom")
print(Kingdom_total)

Phylum_num<-aggregate(MS2.count~Phylum, Phylum_set, length)
names(Phylum_num)[2]<-"num"
Phylum_sum<-aggregate(MS2.count~Phylum, Phylum_set, sum)
names(Phylum_sum)[2]<-"sum"
phylum_total<-merge(Phylum_num, Phylum_sum)
names(phylum_total)[1]<-"name"
phylum_total<-cbind(phylum_total, lca = "phylum")
#print(phylum_total)

Class_num<-aggregate(MS2.count~Class, Class_set, length)
names(Class_num)[2]<-"num"
Class_sum<-aggregate(MS2.count~Class, Class_set, sum)
names(Class_sum)[2]<-"sum"
Class_total<-merge(Class_num, Class_sum)
names(Class_total)[1]<-"name"
Class_total<-cbind(Class_total, lca = "order")
#print(Class_total)

Order_num<-aggregate(MS2.count~Order, Order_set, length)
names(Order_num)[2]<-"num"
Order_sum<-aggregate(MS2.count~Order, Order_set, sum)
names(Order_sum)[2]<-"sum"
Order_total<-merge(Order_num, Order_sum)
names(Order_total)[1]<-"name"
Order_total<-cbind(Order_total, lca = "order")
#print(Order_total)

Family_num<-aggregate(MS2.count~Family, Family_set, length)
names(Family_num)[2]<-"num"
Family_sum<-aggregate(MS2.count~Family, Family_set, sum)
names(Family_sum)[2]<-"sum"
Family_total<-merge(Family_num, Family_sum)
names(Family_total)[1]<-"name"
Family_total<-cbind(Family_total, lca = "family")
#print(Family_total)

Genus_num<-aggregate(MS2.count~Genus, Genus_set, length)
names(Genus_num)[2]<-"num"
Genus_sum<-aggregate(MS2.count~Genus, Genus_set, sum)
names(Genus_sum)[2]<-"sum"
Genus_total<-merge(Genus_num, Genus_sum)
names(Genus_total)[1]<-"name"
Genus_total<-cbind(Genus_total, lca = "genus")
#print(Genus_total)

Species_num<-aggregate(MS2.count~Species, Species_set, length)
names(Species_num)[2]<-"num"
Species_sum<-aggregate(MS2.count~Species, Species_set, sum)
names(Species_sum)[2]<-"sum"
Species_total<-merge(Species_num, Species_sum)
names(Species_total)[1]<-"name"
Species_total<-cbind(Species_total, lca = "species")
#print(Species_total)

df<-rbind(Kingdom_total,phylum_total,Class_total,Order_total,Family_total,Genus_total,Species_total)
write.csv(df, "lala_output.csv")
}