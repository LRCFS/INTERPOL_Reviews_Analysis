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
# This code is for Figure 5
#
###########################################################################

#Clear all lists from memory to avoid unintentional errors

rm(list = ls())

library(ggpubr)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(reshape2)

dat = read.delim('Table2.csv', sep = ",")
ifsms.dat = dat[,1:3]
scopus.dat = dat[,c(1,5:6)]

# take difference of reference counts
# and make long
ifsms.l = ifsms.dat %>% 
  mutate(IFSMS2 = IFSMS - IFSMSandScopus) %>% 
  select(-IFSMS) %>% 
  gather(source, counts, -EvidenceType)

# plot as stacked barplot
plt1 = ggplot(ifsms.l, aes(x = EvidenceType, y = counts, fill = source)) + 
  geom_col() + 
  coord_flip() + 
  scale_fill_manual(labels = c('IFSMS', 'Scopus'), values = brewer.pal(3, 'Blues')[1:2]) + 
  ggtitle('IFSMS Report') + 
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())

# do same as about for scopus searches
scopus.l = scopus.dat %>% 
  mutate(Scopus = ScopusCount - IFSMSCount) %>% 
  select(-ScopusCount) %>% 
  gather(source, counts, -EvidenceType)

plt2 = ggplot(scopus.l, aes(x = EvidenceType, y = counts, fill = source )) + 
  geom_col(position = position_stack(reverse = TRUE)) + 
  coord_flip() + 
  scale_fill_brewer(palette = 'Blues') + 
  labs(title = 'Scopus Search', x = '') + 
  theme_minimal() + 
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank())

# do same as about for scopus searches
scopus.l = scopus.dat %>% 
  mutate(Scopus = ScopusCount - IFSMSCount) %>% 
  select(-ScopusCount) %>% 
  gather(source, counts, -EvidenceType)

plt2 = ggplot(scopus.l, aes(x = EvidenceType, y = counts, fill = rev(source))) + 
  geom_col() + 
  coord_flip() + 
  scale_fill_brewer(palette = 'Blues', direction=-1) + 
  labs(title = 'Scopus Search', x = '') + 
  theme_minimal() + 
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank())

ggarrange(plt1, plt2, common.legend = T, widths = c(1.5,1), legend = 'bottom')

ggsave("Results/Figure5.tiff", width = unit(7.27, 'in'), height = unit(5, 'in'), dpi=300)

print("Processing complete. Please check 'Results' folder for output")