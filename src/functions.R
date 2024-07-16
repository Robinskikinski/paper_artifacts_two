

theme_freq_plots <- function()
{theme_casra(plotColor = goodContrastGrey) +
    theme(
      legend.position = "top",
      legend.justification = "top",
      legend.text = element_text(size = 10),
      axis.line = element_line(colour = goodContrastGrey),
      panel.grid.major.x =  element_line(colour = goodContrastGrey),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      #legend.position = c(0.9,0.25),
      plot.margin = margin(t = 10, r =15, b = 10, l = 0))}


ggplot_freq <- function(data., item., rating., prctg.){
  ggplot(data., aes(
    x = fct_rev(get(item.)),
    fill = fct_rev(factor(get(rating.))),
    y = get(prctg.))) +
    geom_bar( stat = "identity", width = 0.7) +
    geom_text(
      aes(label = ifelse(
        get(prctg.) < 5,
        NA,
        paste0(round(get(prctg.)), "%"))),
      size = 3,
      position = position_stack(vjust = 0.5),
      color = goodContrastGrey) +
    scale_fill_brewer(
      palette = "RdYlGn",
      direction = 1,
      name = "Wertung",
      labels = rev(c(" 1", " 2", " 3", " 4", " 5", " 6", " 7")),
      guide = guide_legend(nrow = 1)) +
    guides(fill = guide_legend(
      nrow = 1,
      label.position = "top",
      label.hjust = 0.5,
      keywidth=2.15,
      reverse = TRUE,
      keyheight = .6)) +
    coord_flip() +
    scale_y_continuous(expand = c(0,0), labels = function(x) paste0(x, "%")) +
    scale_alpha_discrete(range = c(0.5, 1), guide = FALSE) +
    theme_freq_plots()
  
}

create_reg_table <- function(mB, mZ){
  RegTable <- confint(mZ) %>%
    as_tibble(rownames = "Predictor", .name_repair = "universal") %>%
    mutate(CI = paste0("[",round(..2.5..,2), ";",round(..97.5..,2),  "]")) %>%
    select(Predictor, CI)
  
  RegTable$b = round(coefficients(mB),2)
  RegTable$Beta = round(coefficients(mZ),2)
  
  RegTable <- RegTable %>%
    mutate(Z_CI = paste0(Beta, " ", CI))
  
  RegTable <- RegTable %>%
    select(Predictor, b,Beta, Z_CI)
  
  return(RegTable)
}

n_cor_for_oversampling <- function(.nImagesUncor, .Performance){
  
  nImageCorr = case_when(
    .Performance == "Hit" ~ (.nImagesUncor/75) * 90,
    .Performance == "Miss" ~ (.nImagesUncor/25) * 10
  )
  
  return(nImageCorr)
}


fill_empty_ratings <- function(
  .df,
  nameItemVar = "Item",
  nameRatingVar,
  ratingPossList = 1:7){
  
  temp <- expand.grid(
    Item = select(.df, matches(nameItemVar)) %>%
      unlist() %>%
      unique(),
    Rating = ratingPossList
  )
  
  names(temp) <- c(nameItemVar, nameRatingVar)
  
  .df <- .df %>%
    right_join(temp, by = c(nameItemVar, nameRatingVar))
  
  .df <- .df %>%
    mutate(PercentageCorr = replace_na(PercentageCorr, 0))
  
  return(.df)
}

rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

