


###########################################################绘图部分beg###########################################################

# 绘制散点图 加text
df_mdt_proj1$is_weak <- factor(df_mdt_proj1$is_weak)

x_dw <- min(min(df_mdt_proj1$longitude), p_cell$longitude) - 0.0001
x_up <- max(max(df_mdt_proj1$longitude), p_cell$longitude) + 0.0001
y_dw <- min(min(df_mdt_proj1$latitude), p_cell$latitude) - 0.0001
y_up <- max(max(df_mdt_proj1$latitude), p_cell$latitude) + 0.0001
ggplot(data = df_mdt_proj1, aes(x=longitude, y=latitude, shape = is_weak, color = is_weak)) +
  geom_point(size=2) + labs(title="WEAK COVERAGE cell", x='LONGITUDE', y='LATITUDE') +
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text", label = "发射点", x = p_cell$longitude, y = p_cell$latitude, size = 8, colour = "red")+
  xlim(x_dw, x_up)+
  ylim(y_dw, y_up)

  
  
  
  
  
  
###########################################################绘图部分end###########################################################