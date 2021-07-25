
library(tidyverse)
library(WDI)
library(countrycode)
library(janitor)
library(ggrepel)
library(tidytext)

medals_per_million <- read_csv("https://raw.githubusercontent.com/edomt/tokyo2020/main/output/medals_per_million.csv") %>% 
    mutate(iso2c = countrycode(sourcevar = entity, origin = "country.name", destination = "iso2c"),
           iso2c = ifelse(entity == "Kosovo", "XK", iso2c))


# gdp_search <- WDI::WDIsearch("pp")
# # 
# gdp_search %>%
#     as_tibble() %>%
#     filter(str_detect(name, "GDP")) %>% View
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

gdp_ppp_dat <- WDI(indicator='NY.GDP.MKTP.PP.CD', country="all", start = 2000, end=2021, latest = 20) %>% 
    group_by(iso2c) %>% 
    arrange(desc(year)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    clean_names() %>% 
    rename(gdp_ppp = ny_gdp_mktp_pp_cd) %>%
    ## IMF: https://www.imf.org/en/Publications/WEO/weo-database/2021/April/weo-report?c=193,122,124,156,423,935,128,939,172,132,134,174,532,176,178,436,136,158,542,941,946,137,546,181,138,196,142,182,359,135,576,936,961,184,144,146,528,112,111,&s=PPPGDP,&sy=2019&ey=2020&ssm=0&scsm=1&scc=0&ssd=1&ssc=0&sic=0&sort=country&ds=.&br=1
    bind_rows(tibble(iso2c = "TW", country = "Taiwan", gdp_ppp = 1316.049*1e9, year = 2020)) %>%
    select(-country)

gdp_ppp_pc_dat <- WDI(indicator='NY.GDP.PCAP.PP.KD', country="all", start = 2000, end=2021, latest = 20) %>% 
    group_by(iso2c) %>% 
    arrange(desc(year)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    clean_names() %>% 
    rename(gdp_ppp_pc = ny_gdp_pcap_pp_kd) %>%
    ## source: Worldbank via https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita
    bind_rows(tibble(iso2c = "TW", country = "Taiwan", gdp_ppp_pc = 55078, year = 2020)) %>%
    select(-country, -year)

	

medals_dat <- medals_per_million %>% 
    left_join(gdp_ppp_dat) %>% 
    left_join(gdp_ppp_pc_dat) %>% 
    mutate(medals_per_gdp_ppp_1b = medals/gdp_ppp*1e9,
           medals_per_gdp_ppp_pc = medals/gdp_ppp_pc*1000) 

medals_dat %>% 
    ggplot(aes(medals_per_gdp_ppp_1b, medals_per_million)) +
    geom_point() +
    geom_text_repel(aes(label = entity)) +
    theme_minimal()

ggsave(filename = "img/medals_per_gdp_and_per_million.png", width = 12, height = 6)





plot_dat <- medals_dat %>% 
    arrange(desc(medals_per_gdp_ppp_1b)) %>% 
    select(entity, medals, medals_per_gdp_ppp_1b, medals_per_gdp_ppp_pc, medals_per_million, iso2c) %>% 
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
    mutate(
        key = case_when(
            key == "medals" ~ "Most Medals",
            key == "medals_per_gdp_ppp_1b" ~ "Medals per GDP PPP\n(total)",
            key == "medals_per_gdp_ppp_pc" ~ "Medals per GDP PPP\nper 1k inhabitants",
            key == "medals_per_million" ~ "Medals per 1m\ninhabitants"
        ),
        key = fct_relevel(key, c("Most Medals",  "Medals per GDP PPP\nper 1k inhabitants", "Medals per GDP PPP\n(total)", "Medals per 1m\ninhabitants"))) %>%
    # count(key)
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
          plot.subtitle = element_text(size = 22),
          plot.caption = element_text(size = 15), 
          axis.text.x = element_text(size = 20),
          # panel.grid = element_blank()
          ) +
    labs(x = NULL,
         y = NULL,
         subtitle = "Total number of medals won vs. Medals per GDP PPP (per capita) vs. Medals per million people",
         title = "Different ways to rank top teams of 2020 Summer Olympics",
         caption = "\nLatest data update: July 25th 2021. Source: olympics.com, UN World Populations Prospects, World Bank, IMF. Data visualization by Fabio Votta (@favstats)")

ggsave(filename = "img/bump_ranks.png", width = 14, height = 14)

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


plot_dat %>% 
    mutate(rank = case_when(
        entity == "Australia" & key == "medals" ~ 8L,
        entity == "France" & key == "medals" ~ 7L,
        entity == "Taiwan" & key == "medals" ~ 9L,
        entity == "Brazil" & key == "medals" ~ 10L,
        # entity == "Turkey" & key == "medals" ~ 9L,
        T ~ rank
        ),
        key = case_when(
            key == "medals" ~ "Most Medals",
            key == "medals_per_gdp_ppp_1b" ~ "Medals per GDP PPP (in billions)",
            key == "medals_per_gdp_ppp_pc" ~ "Medals per GDP PPP per 1k inhabitants",
            key == "medals_per_million" ~ "Medals per 1m inhabitants"
        ),
        key = fct_relevel(key, c("Most Medals", "Medals per 1m inhabitants", "Medals per GDP PPP per 1k inhabitants", "Medals per GDP PPP (in billions)"))) %>%
    group_by(key) %>% 
    arrange(desc(values)) %>% 
    slice(1:10) %>% 
    mutate(offset_group = values + max(values)*0.08) %>% 
    ungroup() %>% 
    mutate(offset_group = ifelse(key == "Most Medals", values + max(values)*0.05, offset_group),
           val_lab = ifelse(key == "Most Medals", values, specify_decimal(values, 2)),
           val_lab = ifelse(val_lab == "0.00", specify_decimal(values, 3), val_lab),
           key = as.factor(key),
           entity = reorder_within(entity, values, key)) %>% 
    ggplot(aes(entity, values, fill = rank)) +
    geom_col() +    
    scale_x_reordered() +
    facet_wrap(~key, scales = "free", nrow = 2) +
    coord_flip()  + 
    # scale_x_continuous(breaks = df$year %>% unique()) +
    hrbrthemes::theme_ipsum_rc() +
    geom_text(aes(y = offset_group, label = val_lab)) +
    # theme_minimal() +
    scale_fill_viridis_c(option = "C") +
    theme(legend.position = "none",
          plot.title = element_text(size = 22),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(size = 10), 
          axis.text.x = element_text(size = 9),
          # panel.grid = element_blank()
    ) +
    labs(x = NULL,
         y = NULL,
         subtitle = "Total number of medals won vs. Medals per GDP PPP (per capita) vs. Medals per million people",
         title = "Different ways to rank top teams of 2020 Summer Olympics",
         caption = "\nLatest data update: July 25th 2021. Source: olympics.com, UN World Populations Prospects, World Bank, IMF. Data visualization by Fabio Votta (@favstats)")

ggsave(filename = "img/rankings.png", width = 10, height = 8)

