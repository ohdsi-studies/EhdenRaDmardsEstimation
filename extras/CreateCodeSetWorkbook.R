# 10945: [EHDEN RA] DMARDs broad list
# 10946: [EHDEN RA] DMARDs in ACR guideline
# 10947: [EHDEN RA] Inflammatory arthropathies
# 10948: [EHDEN RA] Malignant neoplasms excluding non-melanoma skin cancer
# 10949: [EHDEN RA] Rheumatoid arthritis with history
# 10950: [EHDEN RA] boDMARDs

# 10951: [EHDEN RA] csDMARDs except hydroxychloroquine
# 10952: [EHDEN RA] Hydroxychloroquine

# 10953: [EHDEN RA] csDMARDs except sulfazalazine
# 10954: [EHDEN RA] Sulfasalazine

# 10955: [EHDEN RA] csDMARDs except leflunomide
# 10956: [EHDEN RA] Leflunomide

# 10957: [EHDEN RA] csDMARDs except methotrexate
# 10958: [EHDEN RA] Methotrexate 

# 10959: [EHDEN RA] Opportunistic Infections with tuberculosis
# 10960: [EHDEN RA] Other infections of interest 
# 10961: [EHDEN RA] Serious Infections 
# 10962: [EHDEN Studyathon] Inpatient or ER visit 

# 10963: [EHDEN Studyathon] Malignant neoplasms of colon and rectum 
# 10964: [EHDEN Studyathon] Malignant neoplasms of leukemia 
# 10965: [EHDEN Studyathon] Malignant neoplasms of lymphoma 

# 10966: [EHDEN RA] Leukopenia
# 10967: [EHDEN RA] Pancytopenia

# 10968: [EHDEN Studyathon] Acute myocardial Infarction 
# 10969: [EHDEN Studyathon] Stroke (ischemic or hemorrhagic) 

source("S:/MiscCode/SetEnvironmentVariables.R")
OhdsiRTools::createConceptSetWorkbook(conceptSetIds = c(10945, 10946, 10947, 10948, 10949, 10950, 10951, 10952, 10953, 10954, 10955, 10956, 10957, 10958, 
                                                        10959, 10960, 10961, 10962, 10963, 10964, 10965, 10966, 10967, 10968, 10969), 
                                      workFolder = "G:/StudyResults/EhdenRaDmardsEstimation2",
                                      baseUrl = Sys.getenv("baseUrl"),
                                      included = TRUE,
                                      mapped = TRUE)

