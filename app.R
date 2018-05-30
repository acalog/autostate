library(shiny)
library(dplyr)
library(stringr)
library(lucr)

abbreviateState <- function(df) {
  if (df == "AL" | df == "al") {
    df <- "Alabama"
  } else if (df == "AK" | df == "ak") {
    df <- "Alaska"
  } else if (df == "AZ" | df == "az") {
    df <- "Arizona"
  } else if (df == "AR" | df == "ar") {
    df <- "Arkansas" 
  } else if (df == "CA" | df == "ca") {
    df <- "California"
  } else if (df == "CO" | df == "co") {
    df <- "Colorado"
  } else if (df == "CT" | df == "ct") {
    df <- "Connecticut"
  } else if (df == "DE" | df == "de") {
    df <- "Delaware"
  } else if (df == "FL" | df == "fl") {
    df <- "Florida"
  } else if (df == "GA" | df == "ga") {
    df <- "Georgia"
  } else if (df == "HI" | df == "hi") {
    df <- "Hawaii"
  } else if (df == "ID" | df == "id") {
    df <- "Idaho"
  } else if (df == "IL" | df == "il") {
    df <- "Illinois"
  } else if (df == "IN" | df == "in") {
    df <- "Indiana"
  } else if (df == "IA" | df == "ia") {
    df <- "Iowa" 
  } else if (df == "KS" | df == "ks") {
    df <- "Kansas"
  } else if (df == "KY" | df == "ky") {
    df <- "Kentucky"
  } else if (df == "LA" | df == "la") {
    df <- "Louisiana"
  } else if (df == "ME" | df == "me") {
    df <- "Maine"
  } else if (df == "MD" | df == "md") {
    df <- "Maryland"
  } else if (df == "MA" | df == "ma") {
    df <- "Massachusetts"
  } else if (df == "MI" | df == "mi") {
    df <- "Michigan"
  } else if (df == "MN" | df == "mn") {
    df <- "Minnesota"
  } else if (df == "MS" | df == "ms") {
    df <- "Mississippi"
  } else if (df == "MO" | df == "mo") {
    df <- "Missouri"
  } else if (df == "MT" | df == "mt") {
    df <- "Montana"
  } else if (df == "NE" | df == "ne") {
    df <- "Nebraska"
  } else if (df == "NV" | df == "nv") {
    df <- "Nevada"
  } else if (df == "NH" | df == "nh") {
    df <- "New Hampshire"
  } else if (df == "NJ" | df == "nj") {
    df <- "New Jersey"
  } else if (df == "NY" | df == "ny") {
    df <- "New York"
  } else if (df == "NM" | df == "nm") {
    df <- "New Mexico"
  } else if (df == "NC" | df == "nc") {
    df <- "North Carolina"
  } else if (df == "ND" | df == "nd") {
    df <- "North Dakota"
  } else if (df == "OH" | df == "oh") {
    df <- "Ohio"
  } else if (df == "OK" | df == "ok") {
    df <- "Oklahoma"
  } else if (df == "OR" | df == "or") {
    df <- "Oregon"
  } else if (df == "PA" | df == "pa") {
    df <- "Pennsylvania"
  } else if (df == "RI" | df == "ri") {
    df <- "Rhode Island"
  } else if (df == "SC" | df == "sc") {
    df <- "South Carolina"
  } else if (df == "SD" | df == "sd") {
    df <- "South Dakota"
  } else if (df == "TN" | df == "tn") {
    df <- "Tennessee"
  } else if (df == "TX" | df == "tx") {
    df <- "Texas"
  } else if (df == "UT" | df == "ut") {
    df <- "Utah"
  } else if (df == "VT" | df == "vt") {
    df <- "Vermont"
  } else if (df == "VA" | df == "va") {
    df <- "Virginia"
  } else if (df == "WA" | df == "wa") {
    df <- "Washington"
  } else if (df == "WV" | df == "wv") {
    df <- "West Virginia" 
  } else if (df == "WI" | df == "wi") {
    df <- "Wisconsin"
  } else if (df == "WY" | df == "wy") { 
    df <- "Wyoming"
  } else {
    df <- df
  }
  return(df)
}

num2word <- function(df) {
  if (df == "1") {
    df <- "one"
  } else if (df == "1.5") {
    df <- "one-and-a-half"
  } else if (df == "2") {
    df <- "two"
  } else if (df == "2.5") {
    df <- "two-and-a-half"
  } else if (df == "3") {
    df <- "three"
  } else if (df == "3.5") {
    df <- "three-and-a-half"
  } else if (df == "4") {
    df <- "four" 
  } else if (df == "4.5") {
    df <- "four-and-a-half"
  } else if (df == "5") {
    df <- "five"
  } else if (df == "5.5") {
    df <- "five-and-a-half"
  } else if (df == "6") {
    df <- "six"
  } else if (df == "6.5") {
    df <- "six-and-a-half"
  } else if (df == "7") {
    df <- "seven"
  } else if (df == "7.5") {
    df <- "seven-and-a-half"
  } else if (df == "8") {
    df <- "eight"
  } else if (df == "8.5") {
    df <- "eight-and-a-half"
  } else if (df == "9") {
    df <- "nine"
  } else if (df == "9.5") {
    df <- "nine-and-a-half"
  } else if (df == "10") {
    df <- "ten"
  } else {
    df <- df
  }
  return(df)
}

server <- function(input, output, session) {
  
  # main fuctions to create real estate statements. 
  record <- reactive({
    if (input$propuse == "Primary") {
      a <- paste0("Primary residence: ", input$address, ifelse(input$class=="condominium", paste(" #", input$apt, ", ", sep=""), ", "), input$city, ", ", abbreviateState(input$state), ", a ", input$class, " purchased in ",
                  input$purchaseyear, ifelse(input$purchaseprice=="", " for an unknown amount", paste0(" for ", to_currency(as.integer(input$purchaseprice), currency_symbol="$"))), ".")
      b <- paste0(ifelse(input$reno == TRUE, " Renovated in " ," Built in "), input$built, ", this property has ", num2word(input$beds), ifelse(input$beds==1, " bedroom, ", " bedrooms, "), 
                  num2word(input$baths), ifelse(input$baths==1, " bathroom", " bathrooms"), 
                  ifelse(input$halfbaths == "", "", paste0(", ", num2word(input$halfbaths))), ifelse(input$halfbaths == 1, " half-bath", 
                                                                                                     ifelse(input$halfbaths > 1, " half-baths", ""))
                  , ", and ", input$living, " square feet of living area.")
      
      c <- paste0(" It was assessed in ", input$assessmentyear, " for ", to_currency(as.integer(input$assessedvalue), currency_symbol="$"), ", and as of ", format(Sys.Date(), "%B %Y"), 
                  ifelse(input$sale == TRUE, ", this property was listed on the market for ", ", it had an estimated market value of "), to_currency(as.integer(input$market), currency_symbol="$"), ".")
      d <- paste0(a, b, c)
    } else if (input$propuse == "Seasonal") {
      a <- paste0("Seasonal property: ", input$address, ifelse(input$class=="condominium", paste(" #", input$apt, ", ", sep=""), ", "), input$city, ", ", abbreviateState(input$state), ", a ", input$class, " purchased in ",
                  input$purchaseyear, ifelse(input$purchaseprice=="", " for an unknown amount", paste0(" for ", to_currency(as.integer(input$purchaseprice), currency_symbol="$"))), ".")
      b <- paste0(" Built in ", input$built, ", this property has ", num2word(input$beds), ifelse(input$beds==1, " bedroom, ", " bedrooms, "), 
                  num2word(input$baths), ifelse(input$baths==1, " bathroom", " bathrooms"), ", and ", input$living, " square feet of living area.")
      c <- paste0(" It was assessed in ", input$assessmentyear, " for ", to_currency(as.integer(input$assessedvalue), currency_symbol="$"), ", and as of ", format(Sys.Date(), "%B %Y"), 
                  ifelse(input$sale == TRUE, ", this property was listed on the market for ", ", it had an estimated market value of "), to_currency(as.integer(input$market), currency_symbol="$"), ".")
      d <- paste0(a, b, c)
    } else if (input$propuse == "Income") {
      a <- paste0("Income property: ", input$address, ifelse(input$class=="condominium", paste(" #", input$apt, ", ", sep=""), ", "), input$city, ", ", abbreviateState(input$state), ", a ", input$class, " purchased in ",
                  input$purchaseyear, ifelse(input$purchaseprice=="", " for an unknown amount", paste0(" for ", to_currency(as.integer(input$purchaseprice), currency_symbol="$"))), ".")
      b <- paste0(" Built in ", input$built, ", this property has ", num2word(input$beds), ifelse(input$beds==1, " bedroom, ", " bedrooms, "), 
                  num2word(input$baths), ifelse(input$baths==1, " bathroom", " bathrooms"), ", and ", input$living, " square feet of living area.")
      c <- paste0(" It was assessed in ", input$assessmentyear, " for ", to_currency(as.integer(input$assessedvalue), currency_symbol="$"), ", and as of ", format(Sys.Date(), "%B %Y"), 
                  ifelse(input$sale == TRUE, ", this property was listed on the market for ", ", it had an estimated market value of "), to_currency(as.integer(input$market), currency_symbol="$"), "."
                  , " This is likely an income property generating ", to_currency(as.integer(input$inc), currency_symbol = "$"), " in rent per month.")
      d <- paste0(a, b, c)
    } else if (input$propuse == "Other") {
      a <- paste0("Other: ", input$address, ", a ", input$class, " purchased in ", input$purchaseyear,ifelse(input$purchaseprice=="", " for an unknown amount",
                                                                                                             paste0(" for ", to_currency(as.integer(input$purchaseprice), currency_symbol="$"))), ".")
      b <- paste0(" It was assessed in ", input$assessmentyear, " for ", input$assessedvalue, ", and as of ", format(Sys.Date(), "%B %Y"), 
                  ", it had an estimated market value of ", input$market)
      d <- paste0(a, b)
    }
    return(d)
  })
  
  # format address into zillow url link
  marketlink <- reactive({
    a <- str_replace(input$address, " ", "-")
    b <- paste0(",-", input$city, ",-", input$state, "_rb/")
    c <- paste0(a, b)
    return(c)
  })
  
  # link to zillow.com/address entered by user
  output$link <- renderUI({
    tags$a(href=paste0("https://www.zillow.com/homes/", marketlink()), target="_blank", "Market Value")
  })
  
  # create link to google search of user-inputted address
  output$assess <- renderUI({
    tags$a(href=paste0("https://www.google.com/search?&source=hp&q=", input$city, "+", input$state, "+assessor")
           , target="_blank", "Assessor Link")
  })
  
  # render apt# field if condo is chosen
  output$apt <- renderUI({
    if (input$class == "condominium") {
      textInput("apt", "Apt#: ")
    }
  })
  
  # render inputbox for rental income if 'income property' box is check
  output$income <- renderUI({
    if (input$propuse == "Income") {
      textInput("inc", "Estimated Monthly Rent: ")
    }
  })
  
  # functions and outputs for adding acreage information
  output$acreage <- renderUI({
    if (input$acre == TRUE) {
      textInput("acres", "Acreage: enter parcel size")
    }
  })
  
  acreout <- reactive({
      paste0(" This parcel is ", ifelse(input$acres=="1", paste0(input$acres, " acre."), paste0(input$acres, " acres.")))
  })
  
  # output for additional property details
  output$details <- renderText({
    input$add
  })
  
  # output final real estate record
  observeEvent(input$gen, {
    output$entry <- renderText({
      input$gen
      if (input$acre == TRUE) {
        paste0(record(), acreout())
      } else {
        record()
      }
    })
  })
  
  
}



ui <- fluidPage(
  h1("Real Estate Helper"),
  h4("Fill out the fields below and press 'Generate' to create output."),
  fluidRow(
    column(2,
           selectInput("propuse", "Property Use:",
                       choices=c("Primary", "Seasonal", "Income", "Other"))
           )
    ),
  fluidRow(
    column(1, 
      radioButtons("class", "Property Class",
                   choices=list("Single Family Home"="single-family home",
                                "Condominium"="condominium",
                                "Multi-Family Building"="multi-family building",
                                "Farm/Agricultural"="farm-use",
                                "Vacant"="vacant parcel"))
      
           ),
    column(10, offset = 1,
           htmlOutput("entry"),
           htmlOutput("details"))),
  fluidRow(
    column(3, wellPanel(
      textInput("address", "Address:"),
      uiOutput("apt"),
      textInput("city", "City:"),
      textInput("state", "State:"),
      uiOutput("income"),
      htmlOutput("link"),
      br(),
      htmlOutput("assess")
      
    )),
    column(3, wellPanel(
      textInput("purchaseyear", "Purchase Year:"),
      textInput("purchaseprice", "Purchase Price:"),
      textInput("assessmentyear", "Assessment Year:"),
      textInput("assessedvalue", "Last Assessed Value:"),
      textInput("market", "Market Value:")
    )),
    column(3, wellPanel(
      textInput("built", "Year Built:"),
      textInput("beds", "Bedrooms:"),
      textInput("baths", "Bathrooms:"),
      textInput("halfbaths", "Half-Baths:"),
      textInput("living", "Living Area:")
    )),
    column(3, wellPanel(
      checkboxGroupInput("add", "Additional Characteristics:", 
                         choices=list("Waterfront" = " This is a waterfront property.", "Development Potential" = 
                                        " It appears the property also has development potential."),
                         selected=NULL),
      checkboxInput("acre", "Acreage: ", value=FALSE),
      checkboxInput("sale", "For Sale:", value=FALSE),
      checkboxInput("reno", "Renovated:", value=FALSE),
      uiOutput("acreage")
      
    ),
    actionButton("gen", "Generate Record")))

  )

shinyApp(ui=ui, server=server)