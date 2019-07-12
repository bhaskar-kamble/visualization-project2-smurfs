bundSV_mfh_sfh_df <- rbind(bundSV_mfh_df, bundSV_sfh_df)


bundSV_mfh_sfh_df$gtype <- c(rep('MFH', 17), rep('SFH', 17))
bundSV_mfh_sfh_df
lm1 <- lm(as.matrix(bundSV_mfh_sfh_df[states[1]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm2 <- lm(as.matrix(bundSV_mfh_sfh_df[states[2]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm3 <- lm(as.matrix(bundSV_mfh_sfh_df[states[3]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm4 <- lm(as.matrix(bundSV_mfh_sfh_df[states[4]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm5 <- lm(as.matrix(bundSV_mfh_sfh_df[states[5]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm6 <- lm(as.matrix(bundSV_mfh_sfh_df[states[6]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm7 <- lm(as.matrix(bundSV_mfh_sfh_df[states[7]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm8 <- lm(as.matrix(bundSV_mfh_sfh_df[states[8]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm9 <- lm(as.matrix(bundSV_mfh_sfh_df[states[9]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm10 <- lm(as.matrix(bundSV_mfh_sfh_df[states[10]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm11 <- lm(as.matrix(bundSV_mfh_sfh_df[states[11]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm12 <- lm(as.matrix(bundSV_mfh_sfh_df[states[12]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm13 <- lm(as.matrix(bundSV_mfh_sfh_df[states[13]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm14 <- lm(as.matrix(bundSV_mfh_sfh_df[states[14]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm15 <- lm(as.matrix(bundSV_mfh_sfh_df[states[15]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
lm16 <- lm(as.matrix(bundSV_mfh_sfh_df[states[16]] ) ~ abrechnungsjahr*as.factor(gtype), data = bundSV_mfh_sfh_df )
as.logical(as.numeric(as.factor(bundSV_mfh_sfh_df$gtype))-1)

p <- plot_ly(bundSV_mfh_sfh_df , x = rep(abrechnungsjahr, 2), 
             color = ~as.logical(as.numeric(as.factor(gtype))-1),  colors = c("#132B43", "#56B1F7")) %>%
  add_markers(y = ~get(states[1]), name = " ",marker=list(color="black")) %>%
  add_lines(y = ~fitted(lm1),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F)
%>%
  
  add_markers(y = ~get(states[2]), name = " " , marker=list(color="black"), visible = F) %>%
  add_lines(y = ~fitted(lm2),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F) %>%
  
  add_markers(y = ~get(states[3]), name = " " , marker=list(color="black"), visible = F) %>%
  add_lines(y = ~fitted(lm3),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F) %>%
  add_markers(y = ~get(states[4]), name = " " , marker=list(color="black"), visible = F) %>%
  add_lines(y = ~fitted(lm4),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F) %>%
  
  add_markers(y = ~get(states[5]), name = " " , marker=list(color="black"), visible = F) %>%
  add_lines(y = ~fitted(lm5),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F) %>%
  
  add_markers(y = ~get(states[6]), name = " " , marker=list(color="black"), visible = F) %>%
  add_lines(y = ~fitted(lm6),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F) %>%
  
  add_markers(y = ~get(states[7]), name = " " , marker=list(color="black"), visible = F) %>%
  add_lines(y = ~fitted(lm7),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F) %>%
  
  add_markers(y = ~get(states[8]), name = " " , marker=list(color="black"), visible = F) %>%
  add_lines(y = ~fitted(lm8),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F) %>%
  
  add_markers(y = ~get(states[9]), name = " " , marker=list(color="black"), visible = F) %>%
  add_lines(y = ~fitted(lm9),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F) %>%
  
  add_markers(y = ~get(states[10]), name = " " , marker=list(color="black"), visible = F) %>%
  add_lines(y = ~fitted(lm10),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F) %>%
  
  add_markers(y = ~get(states[11]), name = " " , marker=list(color="black"), visible = F) %>%
  add_lines(y = ~fitted(lm11),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F) %>%
  
  add_markers(y = ~get(states[12]), name = " " , marker=list(color="black"), visible = F) %>%
  add_lines(y = ~fitted(lm12),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F) %>%
  
  add_markers(y = ~get(states[13]), name = " " , marker=list(color="black"), visible = F) %>%
  add_lines(y = ~fitted(lm13),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F) %>%
  
  add_markers(y = ~get(states[14]), name = " " , marker=list(color="black"), visible = F) %>%
  add_lines(y = ~fitted(lm14),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F) %>%
  
  add_markers(y = ~get(states[15]), name = " " , marker=list(color="black"), visible = F) %>%
  add_lines(y = ~fitted(lm15),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F) %>%
  
  add_markers(y = ~get(states[16]), name = " " , marker=list(color="black"), visible = F)%>%
  add_lines(y = ~fitted(lm16),
            line = list(color = '#07A4B5'), showlegend = TRUE, visible = F)

prep('MFH', 34)
p
