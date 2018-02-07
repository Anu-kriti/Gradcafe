library(stringr)
library(data.table)
gc_data <- readRDS("Data/gradcafe_data.RDS")
gc_data[, Program := "Biostatistics"]
gc_data[, DateContacted := as.Date(DateContacted, "%d %b %Y")]
gc_data[, (14:18) := lapply(.SD, as.numeric), .SDcols = 14:18]

# clean institution
gc_data[grepl("University Of Florida|UFL|UGL", Institution), Institution := "University Of Florida"]
gc_data[grepl("University Of South Florida", Institution), Institution := "University Of South Florida"]
gc_data[grepl("University Of Washington|Waashington|Wasington|UW Seattle", Institution), Institution := "University Of Washington"]
gc_data[grepl("CUNY|University At Buffalo", Institution), Institution := "City University of New York"]
gc_data[grepl("ucla|los angeles", tolower(Institution)), Institution := "University Of California, Los Angeles"]
gc_data[grepl("SUNY|Sunny|University At Albany", Institution) | (grepl("y of new york", tolower(Institution)) & !grepl("city", tolower(Institution))), Institution := "State University of New York"]
gc_data[grepl("Berkeley|Berkely", Institution), Institution := "University Of California, Berkeley"]
gc_data[grepl("boston", tolower(Institution)), Institution := "Boston University"]
gc_data[grepl("brown uni", tolower(Institution)) | tolower(Institution) == "brown", Institution := "Brown University"]
gc_data[grepl("Cal State East Bay", Institution), Institution := "California State University, East Bay"]
gc_data[grepl("cambridge university|cambridge  uni", tolower(Institution)), Institution := "Cambridge University"]
gc_data[grepl("CMU", Institution), Institution := "Carnegie Mellon University"]
gc_data[grepl("case west", tolower(Institution)), Institution := "Case Western Reserve University"]
gc_data[grepl("chapel hil|unc|of north carolina|carlorina", tolower(Institution)), Institution := "University Of North Carolina, Chapel Hill"]
gc_data[tolower(Institution) %in% c("columbia", "colombia university", "columbia  university", "coluumbia universit", "comlumbia university", "columbia school of public health") | 
          grepl("columbia \\(gsas|columbia \\gsas|columbia uni|columbia mailman", tolower(Institution)), 
        Institution := "Columbia University"]
gc_data[grepl("connecticut", tolower(Institution)), Institution := "University Of Connecticut"]
gc_data[grepl("colorado", tolower(Institution)), Institution := "University Of Colorado"]
gc_data[grepl("drexel", tolower(Institution)), Institution := "Drexel University"]
gc_data[grepl("duke", tolower(Institution)), Institution := "Duke University"]
gc_data[grepl("emory", tolower(Institution)), Institution := "Emory University"]
gc_data[grepl("florida sta", tolower(Institution)), Institution := "Florida State University"]
gc_data[grepl("george washington|gwu", tolower(Institution)), Institution := "George Washington University"]
gc_data[grepl("georgetown", tolower(Institution)), Institution := "Georgetown University"]
gc_data[grepl("michigan|u mich|umich|muchigan", tolower(Institution)) & Institution != "Michigan State University", Institution := "University of Michigan"]
gc_data[grepl("harvard|harvad", tolower(Institution)), Institution := "Harvard University"]
gc_data[grepl("illinois|illionois|uic", tolower(Institution)), Institution := "University of Illinois at Chicago"]
gc_data[grepl("indianapolis|iupui", tolower(Institution)), Institution := "Indiana University Purdue University Indianapolis"]
gc_data[grepl("iowa", tolower(Institution)) & Institution != "Iowa State University", Institution := "University Of Iowa"]
gc_data[grepl("jhu|john", tolower(Institution)), Institution := "Johns Hopkins University"]
gc_data[grepl("amherst|massachusetts", tolower(Institution)), Institution := "University Of Massachusetts, Amherst"]
gc_data[grepl("mcgill|mc gill", tolower(Institution)), Institution := "McGill Univerisity"]
gc_data[grepl("houston|university of texas", tolower(Institution)), Institution := "University of Texas, Houston"]
gc_data[grepl("medical university|musc", tolower(Institution)), Institution := "Medical University Of South Carolina"]
gc_data[grepl("minnesota|umn", tolower(Institution)), Institution := "University of Minnesota"]
gc_data[grepl("ncsu", tolower(Institution)), Institution := "North Carolina State University"]
gc_data[grepl("new york uni|nyu", tolower(Institution)), Institution := "New York University"]
gc_data[grepl("northwestern", tolower(Institution)), Institution := "Northwestern University"]
gc_data[grepl("ohio state", tolower(Institution)), Institution := "Ohio State University"]
gc_data[grepl("oregon", tolower(Institution)), Institution := "Oregon Health And Science University"]
gc_data[grepl("university of pennsylvania|upenn|u penn|pennslyvania", tolower(Institution)), Institution := "University Of Pennsylvania"]
gc_data[grepl("penn state|pennstate|pennsylvania state", tolower(Institution)), Institution := "Pennsylvania State University"]
gc_data[grepl("pitt", tolower(Institution)), Institution := "University of Pittsburg"]
gc_data[grepl("queens", tolower(Institution)), Institution := "Queens University"]
gc_data[grepl("rochester", tolower(Institution)), Institution := "University of Rochester"]
gc_data[grepl("rutgers", tolower(Institution)), Institution := "Rutgers University"]
gc_data[grepl("san diego state|ucsd", tolower(Institution)), Institution := "San Diego State University"]
gc_data[grepl("smu|southern methodis", tolower(Institution)), Institution := "Southern Methodist University"]
gc_data[grepl("y of new york", tolower(Institution)) & !grepl("city", tolower(Institution)), table(Institution)]
gc_data[grepl("tamu", tolower(Institution)), Institution := "Texas A&M University"]
gc_data[grepl("alabama", tolower(Institution)), Institution := "University Of Alabama At Birmingham"]
gc_data[grepl("arizona", tolower(Institution)), Institution := "University Of Arizona"]
gc_data[Institution == "Chicago" | grepl("of chicago", tolower(Institution)), Institution := "University Of Chicago"]
gc_data[grepl("ottawa", tolower(Institution)), Institution := "University Of Ottawa"]
gc_data[grepl("southern california|south california", tolower(Institution)), Institution := "University Of Southern California"]
gc_data[grepl("toronto", tolower(Institution)), Institution := "University Of Toronto"]
gc_data[grepl("waterloo|watertloo", tolower(Institution)), Institution := "University Of Waterloo"]
gc_data[grepl("western ontario", tolower(Institution)), Institution := "University Of Western Ontario"]
gc_data[grepl("madison", tolower(Institution)) | Institution %in% c("Wisconsin", "U Of Wisconsin", "University Of Wisconsin"), Institution := "University Of Wisconsin, Madison"]
gc_data[grepl("tufts", tolower(Institution)), Institution := "Tufts University"]
gc_data[grepl("tulane", tolower(Institution)), Institution := "Tulane University"]
gc_data[grepl("south carolina", tolower(Institution)), Institution := "University Of South Carolina"]
gc_data[grepl("of washington", tolower(Institution)) | Institution == "Washington", Institution := "University Of Washington"]
gc_data[grepl("milwaukee", tolower(Institution)), Institution := "University Of Wisconsin, Milwaukee"]
gc_data[grepl("uc davi|davis", tolower(Institution)), Institution := "University of California, Davis"]
gc_data[grepl("umd|maryland", tolower(Institution)), Institution := "University of Maryland"]
gc_data[grepl("alberta", tolower(Institution)), Institution := "University Of Alberta"]
gc_data[grepl("san diego", tolower(Institution)) & !grepl("State", Institution), Institution := "University Of California, San Diego"]
gc_data[grepl("kansas", tolower(Institution)), Institution := "University Of Kansas"]
gc_data[grepl("north texas", tolower(Institution)), Institution := "University Of North Texas"]
gc_data[grepl("vanderbilt", tolower(Institution)), Institution := "Vanderbilt University"]
gc_data[grepl("vcu|virginia common|virgina", tolower(Institution)), Institution := "Virginia Commonwealth University"]
gc_data[grepl("washington university|wustl", tolower(Institution)) & !grepl("George", Institution), Institution := "Washington University In St. Louis"]
gc_data[grepl("yale", tolower(Institution)), Institution := "Yale University"]

# status
gc_data[, Status := ifelse(St1 %in% c("A", "I", "O", "U"), St1, NA)]
# season
gc_data[, Season := paste0(ifelse(grepl("f", Season), "Fall ", ifelse(grepl("s", Season), "Spring ", NA)), "20", str_extract(Season, "[0-9]{2}"))]
gc_data[grepl("NA", Season), Season := NA]
# degree
gc_data[, Degree := ifelse(grepl("masters", Degree), "Masters", 
                           ifelse(grepl("phd", Degree), "PhD", 
                                  ifelse(grepl("other", Degree), "Other", NA)))]
# decision
gc_data[Decision == "", Decision := NA]
# contact mode
gc_data[ContactMode == "", ContactMode := NA]
gc_data[ContactMode == "E-Mail", ContactMode := "E-mail"]
# convert old gre scores to new
old_gre <- data.table(
  old = seq(from = 800, to = 230, by = -10),
  new_v = c(rep(170, 5), 169, 169, 168, 168, 167, 166, 165, 165, 164, 164, 163, 162, 162, 161, 160, 160, 
            159, 158, 158, 157, 156, 156, 155, 154, 154, 153, 152, 152, 151, 151, 150, 149, 149, 148, 147, 
            146, 146, 145, 144, 143, 143, 142, 141, 140, 139, 138, 137, 135, 134, 133, 132, 131, 130), 
  new_q <- c(166, 164, 163, 161, 160, 159, 158, 157, 156, 155, 155, 154, 153, 153, 152, 151, 151, 150,
             149, 149, 148, 148, 147, 147, 146, 146, 145, 145, 144, 144, 144, 143, 143, 142, 142, 141, 
             141, 141, 140, 140, 140, 139, 139, 138, 138, 138, 137, 137, 136, 136, 136, 135, 135, 134, 
             134, 133, 133, 132)
)

gc_data <- old_gre[, .(V = old, new_v)][gc_data, on = .(V)]
gc_data <- old_gre[, .(Q = old, new_q)][gc_data, on = .(Q)]
gc_data[, V := ifelse(V < 200, V, new_v)]
gc_data[, Q := ifelse(Q < 200, Q, new_q)]

saveRDS(gc_data, "Data/clean_gradcafe_20180206.RDS")