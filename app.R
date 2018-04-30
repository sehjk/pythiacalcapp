library(shiny)

# Fluid page lays out your inputs and outputs and accepts inputs.
ui <- fluidPage(

  titlePanel(title = div(img(src="dihi.png", width = "25%"),"Post-operative Complication Risk Calculator"),
             windowTitle = "Post-operative Complication Risk Calculator"),

  hr(),

  fluidRow(
    column(
      width = 6,
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
      ),
      br(),
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
      ),
      br(),
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
    ),

    column(
      width = 6,
      h3("Risk Prediction"),
      em("*Complication defined as death or the diagnosis of one of pre-defined ICD codes within 30 days of procedure. Only showing those complications with greater than 5% risk."),
      br(),
      hr(),
      br(),
      h4("Complication Risk"),
      br(),
      tableOutput('risk_table')
    )
  )
)

# calculates your outputs and any other calculations needed for outputs
server <- function(input, output){

  crosswalk <- readRDS("data/ccs_crosswalk.rds")
  coef_table <- readRDS("data/model_coefs_pythia.rds")
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
        coef_sums_interim <- sum(coef_table[coef_table$coef %in% c(input$race, input$meds, input$comorbidities, input$smoke, input$sex, ccsLabel), "death"])
        lp <- exp(sum(coef_table[which(coef_table$coef == "(Intercept)"), "death"])
                  + input$age*sum(coef_table[which(coef_table$coef == "AGE"), "death"])
                  + n_meds*sum(coef_table[which(coef_table$coef == "N_MEDS"), "death"])
                  + input$bmi*sum(coef_table[which(coef_table$coef == "BMI"), "death"])
                  + input$n_cpt*sum(coef_table[which(coef_table$coef == "N_CPT_CODES"), "death"])
                  + input$inpatient*sum(coef_table[which(coef_table$coef == "IN_PATIENT"), "death"])
                  + coef_sums_interim)
        risk <- round((lp/(1+lp))*100,1)
        result <- paste0("Patient's risk for death within 30-days is: ", risk, "%")
      } else {
        result <- "We cannot predict the risk of this procedure."
      }
    } else {
      result <- "Please enter a valid CPT"
    }
    print(result)
  })

  output$risk_table <- renderTable({

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
                    c("Shock complication","shock")), ncol = 2, byrow = T))
        colnames(dict) <- c("Complication", "cx_abbr")
        result <- merge(dict, risk)
        result <- result[, 2:3]
        result <- result[order(-rank(result[["Risk (%)"]])),]
        result$Risk <- ifelse(result[["Risk (%)"]] >= 17 & result[["Complication"]] == "Any complication", "High", "")
        result <- result[which(result[["Risk (%)"]] >= 5), ]
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