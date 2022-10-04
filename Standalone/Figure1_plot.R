###########################################################################
#
# Research trends in forensic science: A scientometric approach to analyse 
# the content of the INTERPOL reviews - Copyright (C) 2021
#
# Leverhulme Research Centre for Forensic Science

# Centre for Forensic Science, Department of Pure and Applied Chemistry,
# University of Strathclyde, Royal College, 204 George Street, Glasgow

# Hervé Ménard, Oyewumi Akinpelu, Nana A. Fiakpui, Rong (Lily) He, Sarah Huxter,
# Caitlin Jordan, Lucy Judge, Aoife King, Brianna Miller, Sophie E. Moggs,
# Carmen-Teodora Patrascu, Teri Pearson, Eranthi M.E.J. Seneviatne,
# Lotte E. Timmerman, Penelope R. Haddrill, Joyce K. Klu, Christian Cole,
# Niamh Nic Daéid

# Website: https://github.com/LRCFS/INTERPOL_Reviews_Analysis
# Contact: lrc@dundee.ac.uk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
###########################################################################
#
# This code is for Figure 1
#
###########################################################################

#Clear all lists from memory to avoid unintentional errors

rm(list = ls())

library(ggpubr)
library(RColorBrewer)
library(dplyr)
library(tidyr)

dat = read.csv('Table1.csv')

dat$colour <- as.character(dat$colour)
names(dat)[1] <- c("Evidence")

p <- ggplot(dat, aes(x=Report, fill=colour)) +
  geom_bar(aes(weight = References))  + 
  scale_fill_manual("legend", values = c("0" = "grey", "1" = "blue")) +
  ylab("Number of references") + theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 8),
                                       legend.position = "none") +
  facet_wrap(~Evidence, ncol = 4, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size = 6),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6)) + 
  labs(caption = "** includes references from the 11 IFSS, 12 IFSS and 13 IFSS reports (not shown)", ) +
  theme(plot.caption = element_text(size = 6))

show(p)

ggsave("Results//Table1.png", dpi= 300, width = unit(4.87, 'in'), height = unit(6, 'in'))

print("Processing complete. Please check 'Results' folder for output")
