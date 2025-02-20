#' Monthly max average value weighted returns data.
#'
#' The original data set consists of the daily simple returns of n = 49
#' industry portfolios from 1927 to 2020.
#'
#' @docType data
#'
#' @usage data(MMavwreturn)
#'
#' @format
#' \describe{
#'   \item{data_SS}{Data matrix.}
#'   \item{Lt}{Observation time points.}
#'   \item{Ly}{Daily average value weighted returns.}
#'   \item{weight}{Weights for local linear smotthing.}
#'   ...
#' }
#' @source \url{http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html}
#'
#' @examples
#'

library(FPMD)
data = read.csv('49_industry_Portfolios_Daily.CSV')
#筛选data，保留里面X>199000的
data = data[which(data$X > 20230800),]
# 2. 提取 Lt（时间点）：归一化
#Lt <- rep(list( data$X ), ncol(data) - 1)
#从1/leng(data$X) 到1，步长为1/length(data$X)
Lt <- rep(list(seq(1/length(data$X), 1, by = 1/length(data$X))), ncol(data) - 1)
colnames(data)[1] = 'Date'

#把data$X重构一下，里面的月份如199001变成1991+1/12
data$Date = as.Date(as.character(data$Date), format = "%Y%m%d")

# 3. 提取 Ly（观测值）：去掉 ym 列，并按列转换为列表
Ly <- as.list(as.data.frame(data[, -1]))
MMavwreturn = list(data_SS = data, Lt = Lt, Ly = data$Ly)
data_SS = MMavwreturn$data_SS

