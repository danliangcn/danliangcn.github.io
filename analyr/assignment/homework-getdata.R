library(haven)
cgss <- read_sav("~/Downloads/cgss2015.sav")

### 2. 在Rstudio中将数据导入，选择编号（id）、样本类型（s1）、居住省份（s41）、
### 个人全年总收入（A8a）、出生年份（a301）、
### 性别（A2）、教育程度（A7a）、政治面貌（A10）、工作年限（A59c）、
### 工作经历及状态（A58）、父亲教育程度（A89b）、母亲教育程度（A90b）、
### 父亲的工作单位类型（A89g）、母亲的工作单位类型（A90g）等变量
### 构成新的工作数据集cgssincome，
### 再根据样本类型（s1）变量选取城市的被调查对象（s1值不为2）并保存

cgssincome <- data.frame(id=cgss$id,urban=cgss$s1,province=cgss$s41,
                         income=cgss$a8a,birth=cgss$a301,sex=cgss$a2,edu=cgss$a7a,
                         polstatus=cgss$a10,workexp=cgss$a59c,workstatus=cgss$a58,
                         fatheredu=cgss$a89b,motheredu=cgss$a90b,fatherwork=cgss$a89g,
                         motherwork=cgss$a90g)
cgssincome <- cgssincome[cgssincome$urban!=2,]

setwd("~/Downloads/rproject/presentation")
save(cgssincome, file = "cgssincome.RData")
