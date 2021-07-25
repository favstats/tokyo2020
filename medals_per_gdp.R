
library(tidyverse)
library(WDI)
library(countrycode)
library(janitor)
library(ggrepel)
library(tidytext)

medals_per_million <- read_csv("https://raw.githubusercontent.com/edomt/tokyo2020/main/output/medals_per_million.csv") %>% 
    mutate(iso2c = countrycode(sourcevar = entity, origin = "country.name", destination = "iso2c"),
           iso2c = ifelse(entity == "Kosovo", "XK", iso2c))


# gdp_search <- WDI::WDIsearch("ppp")
# 
# gdp_search %>%
#     as_tibble() %>%
#     filter(str_detect(name, "capita")) %>% 
#     head()

# A tibble: 6 x 2
# indicator            name                                                             
# <chr>                <chr>                                                            
#     1 6.0.GDPpc_constant   "GDP per capita, PPP (constant 2011 international $) "           
# 2 BG.KAC.FNEI.GD.PP.ZS "Gross private capital flows (% of GDP, PPP)"                    
# 3 HF.UHC.OOP.CG        "Mean household per capita out-of-pocket health spending ($ 2011…
# 4 NY.GDP.PCAP.PP.CD    "GDP per capita, PPP (current international $)"                  
# 5 NY.GDP.PCAP.PP.KD    "GDP per capita, PPP (constant 2017 international $)"            
# 6 NY.GDP.PCAP.PP.KD.87 "GDP per capita, PPP (constant 1987 international $)" 

gdp_ppp_dat <- WDI(indicator='NY.GDP.PCAP.PP.KD', country="all", start = 2000, end=2021, latest = 20) %>% 
    group_by(iso2c) %>% 
    arrange(desc(year)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    clean_names() %>% 
    rename(gdp_ppp = ny_gdp_pcap_pp_kd) %>% 
    ## source: Worldbank via https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita
    bind_rows(tibble(iso2c = "TW", country = "Taiwan", gdp_ppp = 55078, year = 2020)) %>% 
    select(-country)


medals_dat <- medals_per_million %>% 
    left_join(gdp_ppp_dat) %>% 
    mutate(medals_per_gdp_ppp_1000 = medals/gdp_ppp*1000) 

medals_dat %>% 
    ggplot(aes(medals_per_gdp_ppp_1000, medals_per_million)) +
    geom_point() +
    geom_text_repel(aes(label = entity)) +
    theme_minimal()

ggsave(filename = "img/medals_per_gdp_and_per_million.png", width = 12, height = 6)





plot_dat <- medals_dat %>% 
    arrange(desc(medals_per_gdp_ppp_1000)) %>% 
    select(entity, medals, medals_per_gdp_ppp_1000, medals_per_million, iso2c) %>% 
    gather(key, values, -entity, -iso2c) %>% 
    group_by(key) %>% 
    mutate(rank = rank(-values, ties.method = "random")) %>% 
    arrange(rank) %>% 
    # slice(1:2) %>%
    ungroup() %>% 
    mutate(iso2c = str_to_lower(iso2c))

hrbrthemes::import_roboto_condensed()

library(randomcoloR)

n <- 40
n_distinct_cols <- distinctColorPalette(n)


plot_dat %>% 
    mutate(key = case_when(
        key == "medals" ~ "Most Medals",
        key == "medals_per_gdp_ppp_1000" ~ "Medals per GDP PPP per Capita",
        key == "medals_per_million" ~ "Medals per Million Inhabitants"
        ),
        key = fct_relevel(key, c("Most Medals", "Medals per GDP PPP per Capita", "Medals per Million Inhabitants"))) %>% 
    ggplot(aes(key, rank, group = entity, color = entity, fill = entity)) +
    geom_bump(aes(smooth = 10), size = 1.5, lineend = "round")  +
    geom_flag(#data = plot_dat %>% filter(key == "medals"), 
              aes(country = iso2c),
              size = 8,
              color = "black") +
    scale_y_reverse(breaks = 1:40) +
    # scale_x_discrete(position = "top")  +
    scale_color_manual(values = n_distinct_cols) + 
    # scale_x_continuous(breaks = df$year %>% unique()) +
    hrbrthemes::theme_ipsum_rc() +
    # theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size = 35),
          plot.subtitle = element_text(size = 25),
          plot.caption = element_text(size = 15), 
          axis.text.x = element_text(size = 20),
          # panel.grid = element_blank()
          ) +
    labs(x = NULL,
         y = NULL,
         subtitle = "Total number of medals won vs. Medals per GDP PPP per capita vs. Medals per million people",
         title = "Different ways to rank top teams of 2020 Summer Olympics",
         caption = "\nLatest data update: July 25th 2021. Source: olympics.com, UN World Populations Prospects, World Bank. Data visualization by Fabio Votta (@favstats)")

ggsave(filename = "img/bump_ranks.png", width = 14, height = 14)


plot_dat %>% 
    mutate(rank = case_when(
        entity == "Australia" & key == "medals" ~ 8L,
        entity == "France" & key == "medals" ~ 7L,
        entity == "Taiwan" & key == "medals" ~ 10L,
        entity == "Turkey" & key == "medals" ~ 9L,
        T ~ rank
        ),
        key = case_when(
            key == "medals" ~ "Most Medals",
            key == "medals_per_gdp_ppp_1000" ~ "Medals per GDP PPP per 1000",
            key == "medals_per_million" ~ "Medals per Million Inhabitants"
        ),
        key = fct_relevel(key, c("Most Medals", "Medals per GDP PPP per 1000", "Medals per Million Inhabitants"))
    ) %>% #filter(entity == "Australia")
    group_by(key) %>% 
    arrange(desc(values)) %>% 
    slice(1:10) %>% 
    mutate(offset_group = values + max(values)*0.08) %>% 
    ungroup() %>% 
    mutate(offset_group = ifelse(key == "Most Medals", values + max(values)*0.05, offset_group),
           key = as.factor(key),
           entity = reorder_within(entity, values, key)) %>% 
    ggplot(aes(entity, values, fill = rank)) +
    geom_col() +    
    scale_x_reordered() +
    facet_wrap(~key, scales = "free") +
    coord_flip()  + 
    # scale_x_continuous(breaks = df$year %>% unique()) +
    hrbrthemes::theme_ipsum_rc() +
    geom_text(aes(y = offset_group, label = round(values, 2))) +
    # theme_minimal() +
    scale_fill_viridis_c(option = "C") +
    theme(legend.position = "none",
          plot.title = element_text(size = 25),
          plot.subtitle = element_text(size = 15),
          plot.caption = element_text(size = 10), 
          axis.text.x = element_text(size = 9),
          # panel.grid = element_blank()
    ) +
    labs(x = NULL,
         y = NULL,
         subtitle = "Total number of medals won vs. Medals per GDP PPP per capita vs. Medals per million people",
         title = "Different ways to rank top teams of 2020 Summer Olympics",
         caption = "\nLatest data update: July 25th 2021. Source: olympics.com, UN World Populations Prospects, World Bank. Data visualization by Fabio Votta (@favstats)")

ggsave(filename = "img/rankings.png", width = 14, height = 6)

