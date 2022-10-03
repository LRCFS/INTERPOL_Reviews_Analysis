#A view is the client server code for what should be displayed to the user
#Each view should only have the code for one shinydashboard tab where possible

view_FrontPage = tabItem(tabName = "view_FrontPage",
                         
                         box(title="Overview", width = 7,
                             p("The use of forensic evidence has become indispensable in many countries and jurisdictions around the world, however the dissemination of research advancements does not necessarily directly or easily reach the forensic science community."),
                             p("Reports from the INTERPOL International Forensic Science Managers Symposium outline major areas that are of interest to forensic practitioners across the INTERPOL member countries. The information contained in the INTERPOL reports is extensive but can be challenging to process."),
                             p("The purpose of this R-Shiny application is to make this information more accessible and to allow its user to export the reference lists based on keyword searches."),
                             p("References relating to 13 evidence types retrieved from the 14th to 19th INTERPOL IFSMS reports (2004-2019) are currently included in this application, either forming a group on their own or split into specific areas creating a total of 13 reference lists:"),
                             tags$ul(
                               tags$li("forensic geoscience"),
                               tags$li("toolmarks"),
                               tags$li("paint and glass"),
                               tags$li("fire investigation"),
                               tags$li("explosives (analysis)"),
                               tags$li("explosives (scene investigation)"),
                               tags$li("toxicology (challenge-advances)"),
                               tags$li("toxicology (surveillance)"),
                               tags$li("digital evidence"),
                               tags$li("fingermark (composition)"),
                               tags$li("fingermark (detection)"),
                               tags$li("questioned documents"),
                               tags$li("biological evidence combined with DNA."),
                               tags$li("Firearms combined with GSR."),
                               tags$li("Fibres and Textiles."),
                               tags$li("Drugs.")
                             ),
                             p("Please help updating content by sending early INTERPOL IFSMS reports to LRCFSdata@dundee.ac.uk"),
                         ),
                         box(width = 5,
                             HTML("<a href='images/Table1.png' target='_blank'><img src='images/Table1.png' width='100%'></a>")
                         )               
)
