
#' LCA analysis
#'
#' perform aggregation on specified lca level with the count of MS2
#'
#' @export
#' @param data_input data.frame obtained from read.csv
#' @param lcaname name of the taxonomy level, for example "kingdom"
#' @param ms MS.count as input

count_mslca <- function(data_input, lcaname, ms){
	col1<-lcaname
	col2<-ms

	by1<-data_input[[col1]]
	by2<-data_input[[col2]]

	data_num<-aggregate(by2~by1, data_input, length)
	names(data_num)[2]<-"num"
	data_sum<-aggregate(by2~by1, data_input, sum)
	names(data_sum)[2]<-"sum"
	data_total<-merge(data_num, data_sum)
	names(data_total)[1]<-"name"
	data_total<-cbind(data_total, lca = lcaname)
	#rint(data_total)
	data_total
}