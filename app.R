library(shiny)

# Fluid page lays out your inputs and outputs and accepts inputs
ui <- fluidPage(

  titlePanel(title = div(img(src="dihi.png", width = "25%"),"Pythia Risk Calculator"),
             windowTitle = "Pythia Risk Calculator"),

  hr(),

  fluidRow(
    column(
      width = 4,
      h4("1. Procedure Details"),
      textInput(
        inputId = "cpt",
        label = "Primary Procedure:",
        value = "44147",
        placeholder = "Enter CPT Code"
      ),
      textOutput('cpt_test'),
      br(),
      numericInput(
        inputId = "n_cpt",
        label = "Total procedure codes:",
        value = "1",
        min = 1,
        max = 5
      )
    ),
    column(
      width = 4,
      h4("2. Demographic and Social History"),
      numericInput(
        inputId = "age",
        label = "Patient Age:",
        value = "70",
        min = 18,
        max = 120
      ),
      numericInput(
        inputId = "bmi",
        label = "Patient BMI:",
        value = "20",
        min = 5,
        max = 100
      ),
      selectInput(
        inputId = "sex",
        label = "Sex:",
        choices = c("Male" = "Male",
                    "Female" = "Female")
      ),
      selectizeInput(
        inputId = "race",
        label = "Choose Patient's Race:",
        choices = c("American Indian or Alaskan Native" = "RaceAMERICAN INDIAN OR ALASKAN NATIVE",
                    "Asian" = "RaceASIAN",
                    "Black or African American" = "RaceBLACK OR AFRICAN AMERICAN",
                    "White Caucasian" = "RaceCAUCASIAN/WHITE",
                    "Not reported/Declined" = "RaceNOT REPORTED/DECLINED",
                    "Other" = "RaceOTHER",
                    "Unavailable" = "RaceUNAVAILABLE"),
        selected = "RaceCAUCASIAN/WHITE"
      ),
      selectInput(
        inputId = "smoke",
        label = "Level of smoking in past year:",
        choices = c("Unknown" = "SMO_STATUSUnknown If Ever Smoked",
                    "Never Assessed" = "SMO_STATUSNever Assessed",
                    "Never Smoker" = "SMO_STATUSNever Smoker",
                    "Never Smoker, with passive smoke exposure" = "SMO_STATUSPassive Smoke Exposure - Never Smoker",
                    "Former Smoker" = "SMO_STATUSFormer Smoker",
                    "Smoker, Current Status Unknown" = "SMO_STATUSSmoker, Current Status Unknown",
                    "Current Smoker, Some Days" = "SMO_STATUSCurrent Some Day Smoker",
                    "Current Smoker, Light" = "SMO_STATUSLight Tobacco Smoker",
                    "Current Smoker, Heavy" = "SMO_STATUSHeavy Tobacco Smoker"),
        selected = "SMO_STATUSNever Smoker"
      )
    ),
    column(
      width = 4,
      h4("3. Patient Comorbidities"),
      selectizeInput(
        inputId = "comorbidities",
        label = "All Comorbidities:",
        choices = c("Congestive Heart Failure" = "CHF",
                    "Valvular Disease" = "Valvular",
                    "Fluid and Electrolyte Disorders" = "FluidsLytes",
                    "Paralysis" = "Paralysis",
                    "Pulmonary Circulation Disorders" = "PHTN",
                    "Deficiency Anemias" = "Anemia",
                    "Other Neurological Disorders" = "NeuroOther",
                    "Blood Loss Anemias" = "BloodLoss",
                    "Hypertension (Complicated or Uncomplicated)" = "HTN",
                    "Renal Failure" = "Renal",
                    "Weight Loss" = "WeightLoss",
                    "Coagulation Deficiency" = "Coagulopathy",
                    "Chronic Pulmonary Disease" = "Pulmonary",
                    "Psychoses" = "Psychoses",
                    "Diabetes w/ Chronic Complications" = "DMcx",
                    "Diabetes without Chronic Complications" = "DM",
                    "Metastatic Cancer" = "Mets",
                    "Hypothyroidism" = "Hypothyroid",
                    "Liver Disease" = "Liver",
                    "Depression" = "Depression",
                    "Obesity" = "Obesity",
                    "Solid Tumor without Metastasis" = "Tumor",
                    "Peripheral Vascular Disease" = "PVD",
                    "Alcohol Abuse" = "Alcohol",
                    "Drug Abuse" = "Drugs",
                    "Lymphoma" = "Lymphoma",
                    "HIV or AIDS" = "HIV",
                    "Peptic Ulcer Disease" = "PUD",
                    "Rheumatoid Arithritis" = "Rheumatic"
        ),
        multiple = T
      ),
      selectizeInput(
        inputId = "meds",
        label = "All Medications:",
        choices = c("Anticoagulants" = "MedsANTICOAGULANTS",
                    "Anesthetics" = "MedsANESTHETICS",
                    "Antibiotics" = "MedsANTIBIOTICS",
                    "Cardiac medications" = "MedsCARDIAC DRUGS",
                    "Analgesics"  = "MedsANALGESICS",
                    "Anti-Hyperglycemics" = "MedsANTIHYPERGLYCEMICS",
                    "Anti-Neoplastics" = "MedsANTINEOPLASTICS",
                    "Antiplatelets" = "MedsANTIPLATELET DRUGS",
                    "Antivirals" = "MedsANTIVIRALS",
                    "Autonomic drugs" = "MedsAUTONOMIC DRUGS",
                    "Cardiovascular Drugs" = "MedsCARDIOVASCULAR",
                    "CNS Drugs" = "MedsCNS DRUGS",
                    "Diuretics" = "MedsDIURETICS",
                    "Hormones" = "MedsHORMONES",
                    "Immunosuppressants" = "MedsIMMUNOSUPPRESSANTS"
        ),
        multiple = T
      ),
      br(),
      h4("4. Risk Modifiers:"),
      checkboxInput("inpatient", "Inpatient procedure")
    )
  ),
  h3("Complication Risk Score"),
  fluidRow(
    column(
      width = 4,
      selectInput(
        inputId = "cx_coef",
        label = "Type of complication: ",
        choices = c("Any complication" = "any_cx",
                    "Cardiac complication" = "cardiac",
                    "Vascular complication" = "vascular",
                    "Neurological complication" = "neuro",
                    "Renal complication" = "renal",
                    "Endocrine complication" = "endo",
                    "Withdrawal complication" = "etoh",
                    "Falls complication" = "falls",
                    "Gastric complication" = "gastro",
                    "Genitourinary complication" = "Genit",
                    "Hematologic complication" = "hemat",
                    "Integumetary complication" = "integ",
                    "Pulmonary complication" = "pulm",
                    "Sepsis complication" = "sepsis",
                    "Shock complication" = "shock",
                    "Death complication" = "deaths")
      ),
      p(textOutput('risk_score', inline = T))
    ),
    column(
      width = 8,
      h5("By Complication"),
      dataTableOutput('risk_table')
    )
  )
)

# calculates your outputs and any other calculations needed for outputs
server <- function(input, output){

  crosswalk <- readRDS("data/ccs_crosswalk.rds")
  coef_table <- readRDS("data/model_coefs_bmi.rds")
  crosswalk <- as.data.frame(crosswalk)
  coef_table <- as.data.frame(coef_table)

  output$cpt_test <- renderText({
    ccsValid <- input$cpt %in% crosswalk$Code
    ccsLabel <- crosswalk[which(crosswalk[, 1] == input$cpt), "Label"]
    result <- ifelse(!ccsValid, "Please enter a valid CPT",
                     paste0("You have entered: ", input$cpt, ". This procedure belongs to CCS class: ", ccsLabel))
    print(result)
  })


  output$risk_score <- renderText({

    ccsLabel <- crosswalk[which(crosswalk[, 1] == input$cpt), "Label"]
    n_meds <- length(input$meds)

    if(input$cpt %in% crosswalk$Code){
      ccsModeled <- ccsLabel %in% coef_table$coef
      if(ccsModeled){
        coef_sums_interim <- sum(coef_table[coef_table$coef %in% c(input$race, input$meds, input$comorbidities, input$smoke, input$sex, ccsLabel), input$cx_coef])
        lp <- exp(sum(coef_table[which(coef_table$coef == "(Intercept)"), input$cx_coef])
                  + input$age*sum(coef_table[which(coef_table$coef == "AGE"), input$cx_coef])
                  + n_meds*sum(coef_table[which(coef_table$coef == "N_MEDS"), input$cx_coef])
                  + input$bmi*sum(coef_table[which(coef_table$coef == "BMI"), input$cx_coef])
                  + input$n_cpt*sum(coef_table[which(coef_table$coef == "N_CPT_CODES"), input$cx_coef])
                  + input$inpatient*sum(coef_table[which(coef_table$coef == "IN_PATIENT"), input$cx_coef])
                  + coef_sums_interim)
        risk <- round((lp/(1+lp))*100,1)
        result <- paste0("Patient's risk for selected complication is: ", risk, "%")
      } else {
        result <- "We cannot predict the risk of this procedure."
      }
    } else {
      result <- "Please enter a valid CPT"
    }
    print(result)
  })

  output$risk_table <- renderDataTable({

    n_meds <- length(input$meds)
    coef_cols <- which(colnames(coef_table) != "coef")
    ccsLabel <- crosswalk[which(crosswalk[, 1] == input$cpt), "Label"]

    if(input$cpt %in% crosswalk$Code){
      ccsModeled <- ccsLabel %in% coef_table$coef
      if(ccsModeled){
        coef_sums_interim <- colSums(coef_table[coef_table$coef %in% c(input$race, input$meds, input$comorbidities, input$smoke, input$sex, ccsLabel), which(colnames(coef_table) != "coef")])
        lp <- exp(colSums(coef_table[which(coef_table$coef == "(Intercept)"), which(colnames(coef_table) != "coef")])
                  + input$age*colSums(coef_table[which(coef_table$coef == "AGE"), which(colnames(coef_table) != "coef")])
                  + n_meds*colSums(coef_table[which(coef_table$coef == "N_MEDS"), which(colnames(coef_table) != "coef")])
                  + input$bmi*colSums(coef_table[which(coef_table$coef == "BMI"), which(colnames(coef_table) != "coef")])
                  + input$n_cpt*colSums(coef_table[which(coef_table$coef == "N_CPT_CODES"), which(colnames(coef_table) != "coef")])
                  + input$inpatient*colSums(coef_table[which(coef_table$coef == "IN_PATIENT"), which(colnames(coef_table) != "coef")])
                  + coef_sums_interim)
        risk <- round((lp/(1+lp))*100,1)
        risk <- cbind(names(risk), as.data.frame(risk))
        colnames(risk) <- c("cx_abbr", "Risk (%)")
        dict <- data.frame(matrix(c(c("Any complication","any_cx"),
                    c("Cardiac complication","card"),
                    c("Vascular complication","vasc"),
                    c("Neurological complication","neuro"),
                    c("Renal complication","renal"),
                    c("Endocrine complication","endo"),
                    c("Withdrawal complication","etoh"),
                    c("Falls complication","falls"),
                    c("Gastric complication","gastro"),
                    c("Genitourinary complication","genit"),
                    c("Hematologic complication","hemat"),
                    c("Integumetary complication","integ"),
                    c("Pulmonary complication","pulm"),
                    c("Sepsis complication","sepsis"),
                    c("Shock complication","shock"),
                    c("Death complication","death")), ncol = 2, byrow = T))
        colnames(dict) <- c("Complication", "cx_abbr")
        result <- merge(dict, risk)
        result <- result[, 2:3]
        result <- result[order(-rank(result[["Risk (%)"]])),]
      } else {
        result <- "We cannot predict the risk of this procedure."
      }
    } else {
      result <- "Please enter a valid CPT"
    }
    return(result)
  })


}

shinyApp(ui = ui, server = server)