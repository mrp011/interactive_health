################################################################
###################        DISCLAIMER        ###################
################################################################

# There are three functions here:

# icd10_interpret categorizes each icd_10 code into one of around 100 categories
# icd_category_class provides larger classes for the claims based on their category 
# category_clean generalized some of the category names to avoid redundancy when grouped

# in order to add categories to icd10_interpret:
###### define the ICD 10 codes at the appropriate place in the case-when statement (really specific things should be added to the top)
###### ALSO ADD THE NEW CATEGORY TO THE APPRORIATE BODY SYSTEM CATEGORY - don't forget, or it will add a lot of confusion and annoying bookeeping later

# to alter the name of categories in icd10_interpret:
###### Just remember that the icd_category_class and category_clean functions are dumb and have all of the categories from icd10_interpret hard-coded,
###### All alterations to names must be made identically in all three functions. use find/replace when possible to avoid typos. 

################################################################

icd10_interpret <- function(icd_10){
  icd_10_1 <- str_sub(icd_10, 1, 1)
  icd_10_2 <- str_sub(icd_10, 1, 2)
  icd_10_3 <- str_sub(icd_10, 1, 3)
  x <- case_when(
    icd_10_3 %in% c('F10', 'Y90', 'K70') |
      str_sub(icd_10, 1, 5) %in% c('Z71.4', 'K85.2', 'G72.1', 'G31.2', 'T51.0',
                                   'K29.2', 'O35.4', 'K29.2') |
      str_sub(icd_10, 1, 6) %in% c('O99.31') |
      icd_10 %in% c('O99.310', 'K86.0', 'E24.4', 'R78.0', 'Q86.0', 'I42.6', 
                    'P04.3', 'G62.1')                                           ~ 'alcohol use/abuse',
    icd_10_1 == 'F' & icd_10_3 < 'F05' |
      icd_10_3 == 'G30' |
      icd_10 %in% c('G13.2', 'G13.8', 'F06.1', 'F06.8', 'G31.1', 'G31.2', 
                    'G31.01', 'G31.09', 'G91.4', 'G94', 'R41.81', 'R54')        ~ 'alzheimers and dementias',
    icd_10_3 %in% c('N14', 'N15', 'N16', 'N17', 'N18', 
                    'N19', 'N25', 'N26', 'Q61', 'Z49') |
      icd_10_2 %in% c('N0') |
      icd_10 %in% c('N13.1', 'N13.2', 'N13.30', 'N13.39', 'R94.4')              ~ 'kidney disease',
    
    icd_10_3 == 'R10'                                                           ~ 'stomach pain',
    icd_10_3 == 'R07'                                                           ~ 'chest pain',
    icd_10_3 == 'G89'                                                           ~ 'nerve pain',
    str_sub(icd_10, 1, 5) == 'M79.6'                                            ~ 'hand pain',
    str_sub(icd_10, 1, 5) == 'H57.1'                                            ~ 'eye pain',
    str_sub(icd_10, 1, 5) == 'M25.5'                                            ~ 'joint pain',
    icd_10 %in% c('R68.84', 'G50.1')                                            ~ 'jaw pain',
    icd_10_3 %in% c("R10", "R52", "R07", 'G89') |
      str_sub(icd_10, 1, 5) %in% c("M79.6", "H57.1", "M25.5") |
      icd_10 %in% c('R68.84', "G50.1")                                          ~ 'unspecified and/or localized pains',
    
    icd_10_2 == 'D0'                                                            ~ 'pre-cancer tumors',
    icd_10_3 >= 'C00' & icd_10_3 < 'C15'                                        ~ 'cancer: lip/oral cavity and pharynx',
    icd_10_3 >= 'C15' & icd_10_3 < 'C26'                                        ~ 'cancer: digestive organs',
    icd_10_2 == 'C3'                                                            ~ 'cancer: respiratory organs',
    icd_10_3 %in% c('C40', 'C41', 'C90')                                        ~ 'cancer: bone, cartilage, plasma',
    icd_10_3 %in% c('C43', 'C44')                                               ~ 'cancer: skin',
    icd_10_3 >= 'C45' & icd_10_3 < 'C50'                                        ~ 'cancer: soft tissue',
    icd_10_3 >= 'C50' & icd_10_3 < 'C69'                                        ~ 'cancer: genitourinary',
    icd_10_3 %in% c('C70', 'C71', 'C72')                                        ~ 'cancer: brain and nervous system',
    icd_10_3 >= 'C73' & icd_10_3 < 'C76'                                        ~ 'cancer: endocrine glands',
    icd_10_3 %in% c('C7A', 'C7B')                                               ~ 'cancer: neuroendocrine system',
    icd_10_3 >= 'C81' & icd_10_3 < 'C88'                                        ~ 'lymphoma',
    icd_10_3 >= 'C91' & icd_10_3 < 'C95'                                        ~ 'leukemia',
    icd_10_1 == 'C' |
      (icd_10_3 >= 'D00' & icd_10_2 < 'D1') |
      icd_10_3 %in% c('Z08', 'G89.3')                                           ~ 'other cancers',
    
    icd_10_3 %in% c('D45', 'D46')                                               ~ 'cancerous conditions: PCV, MDS',
    icd_10_2 %in% c('D1', 'D2', 'D3') & icd_10_3 < 'D37'                        ~ 'benign tumors',
    icd_10_3 >= 'D37' & icd_10_3 < 'D49'                                        ~ 'tumors of uncertain behavior',
    icd_10_2 >= 'D1' & icd_10_2 < 'D5' |
      (icd_10_3 == 'R97')                                                       ~ 'unclassified tumors',
    
    icd_10_2 >= 'D5' & icd_10_3 < 'D65'                                         ~ 'anemias',
    icd_10_3 >= 'D65' & icd_10_2 < 'D9'                                         ~ 'other blood/blood organ disorders',
    
    icd_10_3 >= 'E08' & icd_10_3 < 'E14'                                        ~ 'diabetes',
    icd_10_2 >= 'E4' & icd_10_2 < 'E7'                                          ~ 'nutrition/obesity',
    icd_10_3 == 'E78'                                                           ~ 'hypercholesterolemia',
    icd_10_1 == 'E'                                                             ~ 'other metabolic/endocrine disorders',
    
    icd_10_2 >= 'F1' & icd_10_2 < 'F2'                                          ~ 'psychoactive substance use disorders',
    icd_10_2 >= 'F2' & icd_10_2 < 'F3'                                          ~ 'schizo and phychotic non-mood disorders',
    icd_10_2 >= 'F3' & icd_10_2 < 'F4'                                          ~ 'affective mood disorders',
    icd_10_2 >= 'F4' & icd_10_3 < 'F43'                                         ~ 'anxiety disorders',
    icd_10_3 == 'F43'                                                           ~ 'stress/adjustment disorders',
    icd_10_2 %in% c("F50", "F52", "F53") |
      icd_10_3 == "R37"                                                         ~ 'eating/sleeping/sexual function disorder',
    icd_10_3 >= 'F53' & icd_10_2 < 'F6' |
      icd_10_3 %in% c('F63', 'F68', 'F69') |
      icd_10_2 == 'F9' & icd_10_3 < 'F99'                                       ~ 'other behavioral disorders',
    icd_10_3 == 'F60'                                                           ~ 'specific personality disorders',
    icd_10_3 %in% c('F64', 'F65', 'F66')                                        ~ 'sexual disorders',
    icd_10_2 == 'F7'                                                            ~ 'intellectual disorders',
    icd_10_2 == 'F8'                                                            ~ 'pervasive developmental disorders',
    icd_10_1 == 'F' | 
      icd_10 == 'Z04.6' |
      (icd_10_3 >= 'R40' & icd_10_3 < 'R47')                                    ~ 'other mental, emotional, or behavior disorders',
    icd_10_3 %in% c('G47', "F51")                                               ~ 'sleeping disorders',
    
    icd_10_2 >= 'G1' & icd_10_3 < 'G33'                                         ~ 'degenerative nerve disorders',
    icd_10_3 == 'G35'                                                           ~ 'multiple sclerosis',
    icd_10_3 %in% c('G36', 'G37')                                               ~ 'demyelinating nerve diseases',
    icd_10_3 == 'G40'                                                           ~ 'epilepsy',
    icd_10_3 == 'G45'                                                           ~ 'transient ischemic attacks - mini strokes',
    icd_10_3 %in% c("G43", "G44", "R51")                                        ~ 'headaches and migraines',
    icd_10_3 == 'G46'                                                           ~ 'stroke, lacunar, and cerebral artery syndrome',
    icd_10_2 == 'G5'                                                            ~ 'nerve, nerve-root, and plexus disorders',
    icd_10_2 =='G6'                                                             ~ 'peripheral nerve disorders',
    icd_10_2 == 'G8' & icd_10_3 < 'G89'                                         ~ 'paralytic syndromes',
    icd_10_1 == 'G'                                                             ~ 'other brain and central nervous system',
    
    icd_10_3 >= 'H40' & icd_10_3 < 'H43'                                        ~ 'glaucoma',
    icd_10_1 == 'H'                                                             ~ 'other vision/eye/ear disorders',
    
    icd_10_2 >= 'I1' & icd_10_2 < 'I2'                                          ~ 'hypertensive disease',
    icd_10_3 >= 'I05' & icd_10_2 < 'I1'                                         ~ 'heart disease - rheumatic',
    icd_10_2 >= 'I2' & icd_10_3 < 'I26'                                         ~ 'heart disease - ischemic/coronary',
    icd_10_3 >= 'I26' & icd_10_3 < 'I27'                                        ~ 'heart disease - pulmonary',
    icd_10_2 >= 'I3' & icd_10_2 < 'I6'                                          ~ 'heart disease - other',
    icd_10_1=='I' | 
      icd_10_2 == 'R0'                                                          ~ 'other heart/circulatory disorders',
    
    icd_10_3 >= 'J09' & icd_10_2 < 'J2'                                         ~ 'influenza and pneumonia',
    icd_10_3 %in% c('J40', 'J41', 'J42', 'J20', 'J21')                          ~ 'bronchitis',
    icd_10_2 >= 'J3' & icd_10_3 < 'J35'                                         ~ 'nasal/sinus disorders',
    icd_10_3 == 'J43'                                                           ~ 'emphysema',
    icd_10_3 == 'J45'                                                           ~ 'asthma',
    icd_10_2 %in% c('J6', 'J7')                                                 ~ 'lung disease, external cause',
    icd_10_1 == 'J'                                                             ~ 'other respiratory disorders',
    
    icd_10_2 == 'K7'                                                            ~ 'liver disease',
    icd_10_2 == 'K4'                                                            ~ 'hernia',
    icd_10_3 %in% c('K50', 'K51', 'K52')                                        ~ 'crohns and colitis',
    icd_10_3 %in% c('K25', 'K26', 'K27', 'K28')                                 ~ 'ulcers',
    icd_10_3 %in% c('K29', 'K30', 'K31')                                        ~ 'stomach disorders',
    icd_10_3 %in% c('K20', 'K21', 'K22', 'K23')                                 ~ 'esophagus disorders',
    icd_10_3 %in% c('K85', 'K86')                                               ~ 'pancreas disorders',
    icd_10_3 %in% c('K80', 'K81', 'K82')                                        ~ 'gallstones and gallbladder disorders',
    icd_10_3 >= 'K55' & icd_10_3 < 'K65'                                        ~ 'intestinal disorders',
    icd_10_1=='K' | 
      (icd_10_2 == 'R1')                                                        ~ 'other digestive system disorders',
    
    icd_10_1=='L' | 
      (icd_10_2 == 'R2' & icd_10_3 < "R24")                                     ~ 'skin disease',
    
    icd_10_3 %in% c('M05', 'M06', 'M08', 'M15', 'M16', 'M17',
                    'M18', 'M19', 'M45', 'M47', 'M48')                          ~ 'arthritis',
    icd_10_3 >= 'M20' & icd_10_3 < 'M26' |
      icd_10_1 == "M" & icd_10_2 <= 'M1' & icd_10_3 != 'M04'                    ~ 'non-arthritis joint disorders',
    icd_10_2 %in% c('M5', 'M4')                                                 ~ 'spinal disorders',
    icd_10_2 %in% c('M6', 'M7')                                                 ~ 'muscle and soft tissue disorders',
    icd_10_2 >= 'M8' & icd_10_3 < 'M95'                                         ~ 'bone and cartilage disorders',
    icd_10_1=='M' | 
      (icd_10_3 >= 'R25' & icd_10_2 < "R3")                                     ~ 'other musculoskeletal disorders',
    
    icd_10_1=='N' | 
      (icd_10_2 == 'R3')                                                        ~ 'other genitourinary disorders',
    
    icd_10_2 == "O0" & icd_10_3 < "O09"                                         ~ 'abortion',
    str_sub(icd_10, 1, 6) %in% c('O99.32', 'O99.33')                            ~ 'drug and tobacco use complicating pregnancy',
    icd_10_3 %in% c("O10", "O11", 'O98', 'O99', 'O9A')                          ~ 'pre-existing conditions complications',
    icd_10_3 >= "O12" & icd_10_3 < 'O17'                                        ~ 'hypertensive conditions due to pregnancy',
    icd_10_2 == "O2"                                                            ~ 'maternal disorders due to pregnancy',
    icd_10_3 >= 'O31' & icd_10_3 < 'O37'                                        ~ 'maternal care for fetal problems',
    icd_10_3 >= 'O37' & icd_10_3 < 'O49'                                        ~ 'maternal care for potential delivery problems',
    icd_10_2 %in% c("O6", 'O7')                                                 ~ 'labor and delivery complications',
    icd_10_3 %in% c('O80', 'O82', 'Z37', 'Z38')                                 ~ 'uncomplicated delivery',
    icd_10_3 >= 'O85' & icd_10_3 < 'O92' & icd_10_3 != 'O9A'                    ~ 'complications of the puerperium',
    icd_10_3 %in% c('Z30', 'Z31')                                               ~ 'reproductive management',
    icd_10_3 %in% c('Z32', 'Z33', 'Z34', 'Z36', 'Z3A', 'Z39')                   ~ 'maternal care without complications',
    icd_10_1 == 'O' | icd_10_2 == 'Z3'                                          ~ 'other reproduction services',
    
    icd_10_1=='P' |
      icd_10_3 == 'Z05'                                                         ~ 'perinatal',
    
    icd_10_1=='Q'                                                               ~ 'inherited malformations',
    
    icd_10_1 %in% c('S', 'T', 'V', 'Y') |
      icd_10_3 %in% c('Z04', 'Z89', 'Z90')                                      ~ 'injury and poison',
    
    icd_10_3 >= 'Z11' & icd_10_3 < 'Z14'                                        ~ 'screenings',
    icd_10_3 >= 'Z00' & icd_10_3 < 'Z11'                                        ~ 'general examinations',
    icd_10_3 %in% c('Z23', 'Z28')                                               ~ 'immunizations',
    icd_10_3 >= 'Z20' & icd_10_3 < 'Z30' | icd_10 == 'Z40'                      ~ 'infection prevention',
    icd_10_3 == 'Z41'                                                           ~ 'non-health-state procedures',
    icd_10_3 >= 'Z42' & icd_10_3 < 'Z52' |
      (icd_10_3 >= 'Z85' & icd_10_3 < 'Z87') |
      (icd_10_3 >= 'Z93' & icd_10_3 < 'Z98')                                    ~ 'aftercare',
    icd_10_1=='Z'                                                               ~ 'other encounters with medical professionals',
    icd_10 == 'AMD.'                                                            ~ 'administrative costs',
    icd_10_1 %in% c('A', 'B')                                                   ~ 'infection care',
    icd_10_2 >= 'R7' & icd_10_3 < 'R99'                                         ~ 'pre-diagnosis diagnostics',
    TRUE                                                                        ~ 'other or unclassified care'
  )
  return(x)
}

icd_category_class <- function(icd_category){
  x <- case_when(
    icd_category %in% c('stomach pain',
                     'chest pain',
                     'nerve pain',
                     'hand pain',
                     'eye pain',
                     'joint pain',
                     'jaw pain',
                     'unspecified and/or localized pains')                      ~ 'undiagnosed pains',
    icd_category %in% c('pre-cancer tumors',
                     'cancer: lip/oral cavity and pharynx',
                     'cancer: digestive organs',
                     'cancer: respiratory organs',
                     'cancer: bone, cartilage, plasma',
                     'cancer: skin',
                     'cancer: soft tissue',
                     'cancer: genitourinary',
                     'cancer: brain and nervous system',
                     'cancer: endocrine glands',
                     'cancer: neuroendocrine system',
                     'lymphoma',
                     'leukemia',
                     'other cancers',
                     'cancerous conditions: PCV, MDS')                          ~ 'cancerous conditions',
    icd_category %in% c('benign tumors',
                     'tumors of uncertain behavior',
                     'unclassified tumors')                                     ~ 'non-cancer/unclassified tumors',
    icd_category %in% c('anemias',
                     'other blood/blood organ disorders')                       ~ 'blood/blood organ disorders',
    icd_category %in% c('diabetes',
                     'nutrition/obesity',
                     'other metabolic/endocrine disorders',
                     'hypercholesterolemia')                                    ~ 'metabolic/endocrine disorders',
    icd_category %in% c('psychoactive substance use disorders',
                     'schizo and phychotic non-mood disorders',
                     'affective mood disorders',
                     'anxiety disorders',
                     'stress/adjustment disorders',
                     'eating/sleeping/sexual function disorder',
                     'other behavioral disorders',
                     'specific personality disorders',
                     'sexual disorders',
                     'intellectual disorders',
                     'pervasive developmental disorders',
                     'other mental, emotional, or behavior disorders',
                     'sleeping disorders')                                      ~ 'mental/emotional or behavioral disorders',
    icd_category %in% c('alzheimers and dementias',
                     'degenerative nerve disorders',
                     'multiple sclerosis',
                     'demyelinating nerve diseases',
                     'epilepsy',
                     'transient ischemic attacks - mini strokes',
                     'headaches and migraines',
                     'stroke, lacunar, and cerebral artery syndrome',
                     'nerve, nerve-root, and plexus disorders',
                     'peripheral nerve disorders',
                     'paralytic syndromes',
                     'other brain and central nervous system')                  ~ 'brain and central nervous system',
    icd_category %in% c('glaucoma',
                     'other vision/eye/ear disorders')                          ~ 'vision/eye/ear disorders',
    icd_category %in% c('hypertensive disease',
                     'heart disease - rheumatic',
                     'heart disease - ischemic/coronary',
                     'heart disease - pulmonary',
                     'heart disease - other',
                     'other heart/circulatory disorders')                       ~ 'heart/circulatory disorders',
    icd_category %in% c('influenza and pneumonia',
                     'bronchitis',
                     'nasal/sinus disorders',
                     'emphysema',
                     'asthma',
                     'lung disease, external cause',
                     'other respiratory disorders')                             ~ 'respiratory disorders',
    icd_category %in% c('liver disease',
                     'hernia',
                     'crohns and colitis',
                     'ulcers',
                     'stomach disorders',
                     'esophagus disorders',
                     'pancreas disorders',
                     'gallstones and gallbladder disorders',
                     'intestinal disorders',
                     'other digestive system disorders')                        ~ 'digestive system disorders',
    icd_category %in% c('arthritis',
                     'non-arthritis joint disorders',
                     'spinal disorders',
                     'muscle and soft tissue disorders',
                     'bone and cartilage disorders',
                     'other musculoskeletal disorders')                         ~ 'musculoskeletal disorders',
    icd_category %in% c('kidney disease',
                     'other genitourinary disorders')                           ~ 'genitourinary disorders',
    icd_category %in% c('abortion',
                     'drug and tobacco use complicating pregnancy',
                     'pre-existing conditions complications',
                     'hypertensive conditions due to pregnancy',
                     'maternal disorders due to pregnancy',
                     'maternal care for fetal problems',
                     'maternal care for potential delivery problems',
                     'labor and delivery complications',
                     'uncomplicated delivery',
                     'complications of the puerperium',
                     'reproduction management',
                     'maternal care without complications',
                     'other reproduction services')                             ~ 'reproduction services',
    icd_category %in% c('screenings',
                     'general examinations',
                     'immunizations',
                     'infection prevention',
                     'non-health-state procedures',
                     'aftercare',
                     'other encounters with medical professionals',
                     'administrative costs',
                     'infection care',
                     'pre-diagnosis diagnostics')                               ~ 'disease inspecific encounters with medical professionals',
    TRUE ~ icd_category
  )
}

category_clean <- function(icd_category){
  x <- case_when(
    icd_category %in% c('other encounters with medical professionals', 
                        'other reproduction services', 
                        'other genitourinary disorders',
                        'other musculoskeletal disorders', 
                        'other digestive system disorders', 
                        'other respiratory disorders', 
                        'other heart/circulatory disorders', 
                        'other brain and central nervous system',
                        'other vision/eye/ear disorders',
                        'other mental, emotional, or behavior disorders',
                        'other metabolic/endocrine disorders',
                        'other blood/blood organ disorders',
                        'other cancers',
                        'unspecified and/or localized pains') ~ 'other',
    TRUE ~ icd_category
  )
}
