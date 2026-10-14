###FSAR fourplot example -- Halibut


library(tidyverse)
library(readr)
library(marea)
library(here)
library(patchwork)

# Load .Rdata file
load(here("data-raw", "FSAR-Halibut", "fsar_halibut.Rdata"))
colnames(fsar_halibut@data)[4:6] <- c("value_value","lower", "upper")

#make plots
pF<-plot(fsar_halibut[
  fsar_halibut@data$panel.category=="Fishing" & fsar_halibut@data$ts.name=="Ut"
], style="ribbon",) +
  labs(title="Fishing Mortality", y="Mortality (1/yr)", subtitle="")

pbiomass<-plot(fsar_halibut[
  fsar_halibut@data$panel.category=="Biomass"  & fsar_halibut@data$ts.name=="HSpred"
], style="ribbon") +
  labs(title="Biomass", y="Biomass (kt)", subtitle="") +
  geom_hline (yintercept=10.5, color="red", linetype="dashed") +
  geom_hline (yintercept=22, color="green", linetype="dashed") 

pcatch<-plot(fsar_halibut[
  fsar_halibut@data$panel.category=="Catch" & fsar_halibut@data$ts.name=="CanadaTotal"
]) +
  labs(title="Fish NAFO div", y="Catch (t)", subtitle="")

pabun<-plot(fsar_halibut[
  fsar_halibut@data$panel.category=="Recruitment" & fsar_halibut@data$ts.name=="RVpred"
], style="ribbon") +
  labs(title="RV Survey (modeled)", y="Abundance (millions)", subtitle="")

#use patchwork to make 4plot
(pcatch+ pbiomass)/(pF + pabun)

