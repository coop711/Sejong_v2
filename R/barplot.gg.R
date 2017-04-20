barplot.gg <-
function(x, position, base_family = "", ggtitle = "", xlab = "", ylab = "", fill.name = ""){
  switch(position,
         stack = barplot.gg.stack(x, base_family = base_family, ggtitle = ggtitle, xlab = xlab, ylab = "", fill.name = ""),
         dodge = barplot.gg.dodge(x, base_family = base_family, ggtitle = ggtitle, xlab = xlab, ylab = "", fill.name = ""),
         fill = barplot.gg.fill(x, base_family = base_family, ggtitle = ggtitle, xlab = xlab, ylab = "", fill.name = ""))
}
barplot.gg.stack <-
function(df, base_family = "", ggtitle = "", xlab = "", ylab = "", fill.name = ""){
x <- df[, 2]
y <- unlist(tapply(df$Freq, x, cumsum))
b1 <- ggplot(df, aes(x = x, y = Freq, fill = vote)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE))
b2 <- b1 +
  theme_bw(base_family = base_family) +
#  theme.kr +
  scale_x_discrete(name = xlab) +
  scale_y_continuous(name = ylab, breaks = NULL) +
  scale_fill_manual(name = fill.name, values = rainbow(2)[2:1], labels = df$vote, guide = guide_legend())
b3 <- b2 +
  geom_text(aes(y = y/2), label = format(df$Freq, big.mark = ","), position = position_stack(reverse = TRUE)) +
  ggtitle(ggtitle)
return(b3)
}
barplot.gg.dodge <-
function(df, base_family = "", ggtitle = "", xlab = "", ylab = "", fill.name = ""){
x <- df[, 2]
y <- unlist(tapply(df$Freq, x, cumsum))
b1 <- ggplot(df, aes(x = x, y = Freq, fill = vote)) +
  geom_bar(stat = "identity", position = "dodge")
b2 <- b1 +
  theme_bw(base_family = base_family) +
#  theme.kr +
  scale_x_discrete(name = xlab) +
  scale_y_continuous(name = ylab, breaks = NULL) +
  scale_fill_manual(name = fill.name, values = rainbow(2)[2:1], labels = df$vote)
  N <- nrow(df)
  index <- as.vector(matrix(1:N, nrow = 2)[2:1, ])
b3 <- b2 +
  geom_text(aes(y = df$Freq/2), label = format(df[index, "Freq"], big.mark = ","), position = position_dodge(width = 0.9)) +
  ggtitle(ggtitle)
return(b3)
}
barplot.gg.fill <-
function(df, base_family = "", ggtitle = "", xlab = "", ylab = "", fill.name = ""){
x <- df[, 2]
y <- unlist(tapply(df$Freq, x, function(x){cumsum(x)/sum(x)}))
b1 <- ggplot(df, aes(x = x, y = Freq, fill = vote)) +
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE))
b2 <- b1 +
  theme_bw(base_family = base_family) +
#  theme.kr +
  scale_x_discrete(name = xlab) +
  scale_y_continuous(name = ylab, breaks = NULL) +
  scale_fill_manual(name = fill.name, values = rainbow(2)[2:1], labels = df$vote, guide = guide_legend())
b3 <- b2 +
  geom_text(aes(y = y/2), label = format(df$Freq, big.mark = ","), position = position_stack(reverse = TRUE)) +
  ggtitle(ggtitle)
return(b3)
}
