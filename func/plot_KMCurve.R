get_color <- function (palette, n = 6) 
{
  if (length(palette) > 1) 
    return(palette)
  switch(tolower(palette), nature = {
    (ggsci::pal_npg("nrc"))(n)
  }, jco = {
    (ggsci::pal_jco("default"))(n)
  }, lancet = {
    (ggsci::pal_lancet("lanonc"))(n)
  }, jama = {
    (ggsci::pal_jama())(n)
  }, jama_classic = {
    head(c("#164870", "#10B4F3", "#FAA935", "#2D292A", "#87AAB9", 
           "#CAC27E", "#818282"), n)
  }, RColorBrewer::brewer.pal(n, "Set1"))
}


plot_KMCurve <- function (clinical, labels, limit = NULL, annot = NULL, color = NULL, 
                             font = "Arial", xlab = "Follow up", ylab = "Survival Probability", 
                             title = NULL, legend.pos = "top", palette = "jama_classic", 
                             risk.table = T, risk.table.ratio = 0.4, anno.pos = "bottom", 
                             anno.x.shift = 0.5) 
{
  time <- clinical[, 1]
  event <- clinical[, 2] == 1
  if (!is.null(limit)) {
    event[time > limit] <- F
    time[time > limit] <- limit
  }
  df <- data.frame(futime = time, fustat = event, group = labels)
  surv <- survival::survfit(survival::Surv(futime, fustat) ~ 
                              group, data = df)
  survstats <- survival::survdiff(survival::Surv(futime, fustat) ~ 
                                    group, data = df)
  survstats$p.value <- 1 - pchisq(survstats$chisq, length(survstats$n) - 
                                    1)
  if (!is.null(color)) {
    if (!is.null(names(color))) {
      labels <- factor(labels, levels = names(color))
    }
  }
  else {
    color <- get_color(palette, n = length(unique(labels)))
  }
  if (class(labels) == "factor") {
    legend.labs <- na.omit(levels(droplevels(labels[!(is.na(time) | 
                                                        is.na(event))])))
  }
  else if (class(labels) == "logical") {
    labels <- factor(labels, levels = c(F, T))
    legend.labs <- na.omit(levels(droplevels(labels)))
  }
  else {
    legend.labs <- na.omit(unique(labels))
    labels <- factor(labels, levels = legend.labs)
  }
  fancy_scientific <- function(l, dig = 3) {
    l <- format(l, digits = dig, scientific = TRUE)
    l <- gsub("^(.*)e", "'\\1'e", l)
    l <- gsub("e", "%*%10^", l)
    parse(text = l)
  }
  p <- survminer::ggsurvplot(surv, data = df, xlab = xlab, break.time.by=30,
                             ylab = ylab, palette = color, legend = legend.pos, legend.labs = legend.labs, 
                             risk.table = risk.table, risk.table.title = element_blank(), 
                             risk.table.y.text = FALSE, ggtheme = cowplot::theme_cowplot(),
                             # xlim = c(0, 360),
                             break.x.by = 12) #,xlim = c(0, 108)
  p$plot <- p$plot + # scale_x_continuous(breaks=seq(0,108,12))+# +coord_cartesian(xlim = c(0,120))
    ggtitle(title) + theme(plot.title = element_text(hjust = 0.5,size = 10), 
                           text = element_text(family = font,size = 10), title = element_text(family = font,size = 10), 
                           axis.text.x = element_text(family = font), legend.title = element_blank(),
                           legend.text = element_text(size = 10))
  anno.text <- ifelse(survstats$p.value == 0, "italic(P)<1%*%10^{-22}", 
                      paste0("italic(P)==", fancy_scientific(survstats$p.value, 
                                                             3)))
  anno.y.shift <- 0
#  if (length(legend.labs) == 2) {
#    hr <- survcomp::hazard.ratio(labels[!(is.na(time) | is.na(event))], 
#                                 time[!(is.na(time) | is.na(event))], event[!(is.na(time) | 
#                                                                                is.na(event))])
#    anno.text <- c(anno.text, sprintf("HR == %3.2f~(%3.2f - %3.2f)", 
#                                      hr$hazard.ratio, hr$lower, hr$upper))
#    anno.y.shift <- c(anno.y.shift + 0.15, 0)
#  }
  if (!is.null(annot)) {
    anno.text <- c(anno.text, annot)
    anno.y.shift <- c(anno.y.shift + 0.15, 0)
  }
  if (anno.pos == "bottom") {
    p$plot <- p$plot + annotate("text", family = font, x = 0, size=5,
                                y = anno.y.shift, label = anno.text, hjust = 0, vjust = 0, 
                                parse = TRUE)
  }
  else {
    p$plot <- p$plot + annotate("text", family = font, x = anno.x.shift * 
                                  max(time, na.rm = T), y = 0.85 + anno.y.shift, label = anno.text, 
                                hjust = 0, vjust = 2,size=10, parse = TRUE)
  }
  if (risk.table) {
    p$table <- p$table + theme(text = element_text(family = font,size = 10), 
                               title = element_text(family = font,size = 10), axis.text = element_text(family = font,size = 10), 
                               axis.title.y = element_blank())# + scale_x_continuous(breaks=seq(0,108,12))
    pp <- plot_grid(plotlist = list(p$plot + theme(axis.title.x = element_blank()), 
                                    p$table + labs(x = xlab)), labels = "", ncol = 1, 
                    align = "v", rel_heights = c(1, risk.table.ratio))
    return(pp)
  }
  else return(p$plot)
}