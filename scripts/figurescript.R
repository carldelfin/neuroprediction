MDA1 <- as.data.frame(importance(baselineModel, type = 1, scale = TRUE))
MDA2 <- as.data.frame(importance(extendedModel, type = 1, scale = TRUE))

#baselineList <- c("age", "agefirstcrime", "PCLSVtotal", "male", "SUD", "PDB", "education", "MDFDR", "prevcrimeall")



# correctly formatted names have to be added manually 
MDA1$names <- c("Age",
                "Age at first crime",
                "PCL:SV total score",
                "Male sex",
                "Substance use disorder",
                "Cluster B personality disorder",
                "Educational attainment",
                "Mental disorder in first-degree relative",
                "Previous criminality")

MDA2$names <- c("Age",
                "Age at first crime",
                "PCL:SV total score",
                "Male sex",
                "Substance use disorder",
                "Cluster B personality disorder",
                "Educational attainment",
                "Mental disorder in first-degree relative",
                "Previous criminality",
                "Right cerebellum rCBF",
                "Left cerebellum rCBF",
                "Right frontal lobe rCBF",
                "Left frontal lobe rCBF",
                "Right temporal lobe rCBF",
                "Left temporal lobe rCBF",
                "Right parietal lobe rCBF",
                "Left parietal lobe rCBF")

# reorder
MDA1$names <- factor(MDA1$names, levels = MDA1$names[order(MDA1$MeanDecreaseAccuracy)])
MDA2$names <- factor(MDA2$names, levels = MDA2$names[order(MDA2$MeanDecreaseAccuracy)])

# create plots
p1 <- 
  ggplot(MDA1) + 
  geom_point(aes(MeanDecreaseAccuracy, names)) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(title = "",
       x = "Mean decrease in accuracy (scaled)",
       y = "") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.background = element_rect(fill = "white", colour = "black"),
        text = element_text(size = textsize),
        axis.text = element_text(size = axissize),
        axis.title = element_text(size = axistitlesize),
        title = element_text(size = titlesize))

p2 <- 
  ggplot(MDA2) +
  geom_point(aes(MeanDecreaseAccuracy, names)) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(title = "",
       x = "Mean decrease in accuracy (scaled)",
       y = "") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.background = element_rect(fill = "white", colour = "black"),
        text = element_text(size = textsize),
        axis.text = element_text(size = axissize),
        axis.title = element_text(size = axistitlesize),
        title = element_text(size = titlesize))