if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = "/opt/spark")
}
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
sparkR.session(master = "local[*]", sparkConfig = list(spark.driver.memory = "2g"))

df <- as.DataFrame(faithful)
sparkR.session(sparkPackages = "com.databricks:spark-avro_2.11:3.0.0")
people <- read.df("./people.json", "json")
head(people)
write.df(people, path = "people.parquet", source = "parquet", mode = "overwrite")
df
head(select(df, "eruptions"))
head(filter(df, df$waiting < 50))