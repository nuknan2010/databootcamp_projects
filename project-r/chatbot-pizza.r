# Homework 01
# chatbot (rule-based)
# ordering pizza 

##################### df:: Choose, Pizza, Drink, Size, Delivery, Payment #####################

# --- df:: df_choose ---
df_choose <- data.frame(
  No          = numeric(0),
  Order       = character(0),
  orderPrice  = numeric(0),
  Size        = numeric(0),
  addPrice    = numeric(0),
  Amount      = numeric(0),
  sumPrice    = numeric(0)
)

# --- df:: df_pizza ---
id_pizza <- 1:10
menu <- c("Double Pepperoni Pizza",
          "Pepperoni Mushroom Pizza", 
          "Hawaiian Pizza", 
          "MeatLover Pizza", 
          "Seafoods Pizza Deluxe",
          "Salmon Deluxe Pizza",
          "CheeseSteak Pizza",
          "Spinach Supreme",
          "Veggie Pizza",
          "White Pizza Deluxe"
         )
price <- c(90, 100, 75, 120, 150, 115, 80, 135, 100, 110)
status <- c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE)
df_pizza <- data.frame(
                        ID     = id_pizza,
                        Menu   = menu,
                        Price  = price,
                        Status = status
                      )

# --- df:: df_drink ---
df_drink <- data.frame(
  ID     = 1:3,
  Menu   = c("drinking water", "Pepsi", "Sprite"),
  Price  = c(15, 20, 20),
  Status = c(TRUE,TRUE,FALSE)
)

# --- df:: df_menu_size ---
df_menu_size <- data.frame(
  ID       = 1:3,
  Size     = c("S", "M", "L"),
  addPrice = c(1, 1.5, 2)
)

# --- df:: df_delivery ---
df_delivery <- data.frame(
  ID        = 1:2,
  Delivery  = c("Pick up at store", "Food Delivery"),
  Price     = c(0, 50)
)

# --- df:: df_menu_payment ---
df_payment <- data.frame(
  ID       = 1:2,
  Payment  = c("Cash", "Bank Transfer"),
  Detail   = c("-", "KTB | 000-XXXX-XXX | Irene Bae")
)

# --- list:: df_menu_payment ---
list_amount <- 1:10

##################################### ORDER #####################################

#============= Func:: Choose Menu =============

# --- func:: func_menu ---
func_menu <- function(df_choose, df_list, textShow) {
  #--- choose menu (Pizza || Drink) ---
  print("============================")
  print( paste(" || Choose your", textShow, "ID || ") )
  print("============================")
  print(df_list)
  choose_menu <- readLines("stdin", n=1)
  if (!choose_menu %in% df_list$ID) {
    print("** Invalid ID! please choose again **")
    func_menu(df_choose, df_list, textShow)
  } else if ( !df_list[ df_list$ID == choose_menu, "Status"] ) {
    print("** Sold out! please choose again **")
    func_menu(df_choose, df_list, textShow)
  } else {
    # add new order to dataframe
    df_choose[nrow(df_choose)+1, c(1, 2, 3)] <- c(nrow(df_choose)+1, 
                                                     df_list[ df_list$ID == choose_menu, "Menu"], 
                                                     df_list[ df_list$ID == choose_menu, "Price"])
    #--- choose size ---
    if (textShow == "Pizza") {
      df_choose <- func_size(df_choose)
    } else {
      df_choose$Size[ nrow(df_choose) ] <- NA
      df_choose$addPrice[ nrow(df_choose) ] <- 1
    }

    #--- choose amount ---
    df_choose <- func_amount(df_choose, textShow)

    #--- choose add more ---
    print("Would you like to add more orders ?")
    print("1. Yes   |   2. No")
    choose_add_more <- readLines("stdin", n=1)   
    if (choose_add_more == 1) {
      func_menu(df_choose, df_list, textShow)
    } else {
      return(df_choose)
    }
  }
}

#============= Func:: Choose Size =============

# --- func:: func_size ---
func_size <- function(df_choose) {
  print("------------------------------")
  print("| Choose your pizza size ID |")
  print("------------------------------")
  
  print(df_menu_size[ , c("ID", "Size")] )
  choose_menu_size <- readLines("stdin", n=1)
  if (!choose_menu_size %in% df_menu_size$ID) {
    print("** Please choose again **")
    func_size()
  } else {
    # update order detail in dataframe (add size)
    df_choose$Size[ nrow(df_choose) ] <- df_menu_size[ df_menu_size$ID == choose_menu_size, "Size" ]
    df_choose$addPrice[ nrow(df_choose) ] <- df_menu_size[ df_menu_size$ID == choose_menu_size, "addPrice" ]
    return(df_choose)
  }
}

#============= Func:: Choose Amount =============

# --- func:: func_amount ---
func_amount <- function(df_choose, textShow) {
  print("-----------------------------")
  print( paste(" | How many", textShow, "to order | ") )
  print("-----------------------------")
  cus_menu_amount <- readLines("stdin", n=1)
  if (!cus_menu_amount %in% list_amount) {
    print("** Please choose again **")
    func_amount(df_choose, textShow)
  } else {
    # update order detail in dataframe (add price and amount)
    df_choose$Amount[ nrow(df_choose) ] <- cus_menu_amount
    df_choose$sumPrice[ nrow(df_choose) ] <- as.double(df_choose[ nrow(df_choose), "orderPrice"]) * as.double(df_choose[ nrow(df_choose), "addPrice"]) * as.double(df_choose[ nrow(df_choose), "Amount"])
    return(df_choose)
  }
}

#============= Func:: ORDER =============

# --- func:: func_order ---
func_order <- function(df_choose) {
  df_choose <- func_menu(df_choose, df_pizza, "Pizza")

  print("Would you like to Drinking ?")
  print("1. Yes  |  2. No")
  cus_select_drink <- readLines("stdin", n=1)
  if (cus_select_drink == 1) {
    df_choose <- func_menu(df_choose, df_drink, "Drink")
  }

  func_confirm(df_choose)
}

##################################### CONFIRM #####################################

#============= Func:: Choose Delivery =============
func_delivery <- function() {
  print("---------------------------------")
  print("| Choose your Delivery ID |")
  print("---------------------------------")
  print(df_delivery[ , c("ID", "Delivery")] )
  cus_menu_delivery <- readLines("stdin", n=1)
  if (!cus_menu_delivery %in% df_delivery$ID) {
    print("** Invalid ID! please choose again **")
    func_delivery()
  } else {
    cat("Your Delivery -> ", df_delivery[ df_delivery$ID == cus_menu_delivery, "Delivery"], "\n")
    return(df_delivery[ df_delivery$ID == cus_menu_delivery, "Price"])
  }
}

#============= Func:: Choose Payment =============

# --- func:: func_payment ---
func_payment <- function() {
  print("---------------------------------")
  print("| Choose your payment method ID |")
  print("---------------------------------")
  print(df_payment[ , c("ID", "Payment")] )
  cus_menu_payment <- readLines("stdin", n=1)
  if (!cus_menu_payment %in% df_payment$ID) {
    print("** Invalid ID! please choose again **")
    func_payment()
  } else {
    cat("Your Payment method -> ", df_payment[ df_payment$ID == cus_menu_payment, "Payment" ], "\n")
    cat("Detail -> ", df_payment[ df_payment$ID == cus_menu_payment, "Detail" ], "\n\n")
  }
}

#============= Func:: CONFIRM =============

# --- func:: func_confirm ---
func_confirm <- function(df_choose) {
  # Confirm
  print("---------------------------------------------------------------")
  print(df_choose)
  print("---------------------------------------------------------------")
  print("Confirm your order ?")
  print("1. Yes  |  2. Cancel and re-order the pizza.  |   3. Cancel and Exit")
  cus_menu_confirm <- readLines("stdin", n=1)
  if (cus_menu_confirm == 1) {
    deliveryPrice = func_delivery()
    func_payment()
    func_summary(df_choose, deliveryPrice)
  } else if (cus_menu_confirm == 2) {  
    df_choose = df_choose[0, ]   # delete all row in dataframe
    func_order(df_choose)
  } else {
    print("Cancel and Exit!")
  }
}

##################################### SUMMARY #####################################

#============= Func:: Summary =============
func_summary <- function(df_choose, deliveryPrice) {
  print("############################ Bill ###############################")
  print(df_choose)
  print("---------------------------------------------------------------")
  subPrice <- sum(df_choose$sumPrice)
  vatPrice <- subPrice * 7 / 100
  TotalPrice <- subPrice + deliveryPrice + vatPrice
  
  cat("Sub Total : ", subPrice, "\n")
  cat("Vat (7%) : ", vatPrice, "\n")
  cat("Delivery Cost : ", deliveryPrice, "\n")
  cat("TOTAL : ", TotalPrice, "\n")
  print("#################################################################")
}

#================================================================================================
#                                        Start program                                          #
#================================================================================================
print("Welcome to our app!")
print("Hello There!")

print("What's your name ?")
cus_name <- readLines("stdin", n=1)
print( paste("Hi", cus_name) )

print("What do you want to order today ?")
print("1. Order Pizza   |   2. Exit")
cus_action <- readLines("stdin", n=1)

if (cus_action == 1) {
  func_order(df_choose)
} else {
  print("Exit!")
}
