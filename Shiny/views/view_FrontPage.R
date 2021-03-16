#A view is the client server code for what should be displayed to the user
#Each view should only have the code for one shinydashboard tab where possible

view_FrontPage = tabItem(tabName = "view_FrontPage",
                        
                        mainPanel(width = 6,
                          p("The use of forensic evidence has become indispensable in many countries and jurisdiction around the world, however the dissemination of research advancements does not necessarily directly or easily reach the forensic science community."),
                          p("Reports from the INTERPOL International Forensic Science Managers Symposium outline major areas that are of interest to forensic practitioners across the INTERPOL member countries. The information contained in the INTERPOL reports is extensive but can be challenging to process."),
                          p("The purpose of this R-Shiny application is to make this information more accessible and to allow its user to export the reference lists based on keyword searches."),
                          p("References relating to 10 evidence types retrieved from the 14th to 19th INTERPOL IFSMS reports (2004-2019) are currently included in this application, either forming a group on their own or split into specific areas creating a total of 13 reference lists:"),
                          p(" - forensic geoscience"),
                          p(" - toolmarks"),
                          p(" - paint and glass"),
                          p(" - fire investigation"),
                          p(" - explosives (analysis)"),
                          p(" - explosives (scene investigation)"),
                          p(" - toxicology (challenge-advances)"),
                          p(" - toxicology (surveillance)"),
                          p(" - digital evidence"),
                          p(" - fingermark (composition)"),
                          p(" - fingermark (detection)"),
                          p(" - questioned documents"),
                          p(" - biological evidence combined with DNA.")
                        ),
                        mainPanel(width = 6,
                          img(src = "Table1.png", width = 495, height = 610
                        )
                        )               
)
