# app.R — Paired Comparison (Thurstone/Bradley–Terry ready) 
# Author: (fill your name)
# How to run: save as app.R then run: shiny::runApp()

# ===== Packages =====
suppressPackageStartupMessages({
  library(googledrive)
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(later)
  library(rsconnect)
})

# google setup
drive_auth(
  email  = "s-hashimoto@seinan-gakuin.jp",
  scopes = "https://www.googleapis.com/auth/drive",
  cache  = ".secrets"        # ← ここにトークンを保存
)


DRIVE_FOLDER_ID <- "1Vu0mGWF7EDFpUh1Zt3lIUdwu_HBJ_rQw"  # 例: '1AbCdEfGhijkLMNO

# ===== User-configurable items =====
# 1) Define the set of items to be compared.
#    You may use only labels, or add an image_url column for pictures.
#    Example below is for 7 characters.
items_tbl <- tibble::tribble(
  ~item_id, ~label,            ~image_url,
  "A",      "Paddington",      "paddington1.jpg",
  "B",      "Simba",           "Simba.png",
  "C",      "Roronoa Zoro",    "Roronoa_Zoro.jpg",
  "D",      "Kyo Sohma",       "Kyo_Sohma.webp",
  "E",      "Dipper Pines",    "dipper_pines.jpg",
  "F",      "Nyaan",           "nyaan.png",
  "G",      "Spider-Man",      "spider.png",
  "H",      "Brian O'Conner",  "Schermafbeelding.png",
  "I",      "Link",            "Link.jpg"
)


# ==== Attributes (各ブロック＝1属性) ====
# ==== Attribute（ここだけ毎回変える）====
ATTR_ID    <- "A1"
ATTR_LABEL <- "Attractive"
ATTR_INSTR <- paste0("Which do you find more ", ATTR_LABEL, ", the left or the right?")

# 2) Experiment settings
APP_TITLE <- paste0("Paired Comparison Experiment \n (Which is more `", ATTR_LABEL ,"'?)")
INSTRUCTION_TEXT <- paste(
  ATTR_INSTR,
  "Choose based on your intuition.",
  "All combinations will be presented once.",
  sep = "\n"
)
# Counterbalancing: each pair is shown once, left/right randomized per trial.
# If you want multiple repetitions, set N_REPS > 1.
N_REPS <- 1
ALLOW_SKIP <- FALSE  # if TRUE, shows a 'スキップ' button

# ===== Utility: make full trial list =====
make_trials <- function(items, n_reps = 1, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  ids <- items$item_id
  pairs <- t(combn(ids, 2)) %>% as.data.frame() %>% setNames(c("i1","i2"))
  trials <- map_dfr(seq_len(n_reps), function(rep_i){
    pairs %>% mutate(rep = rep_i)
  })
  # Randomize left/right assignment per trial
  lr_flip <- sample(c(TRUE, FALSE), nrow(trials), replace = TRUE)
  trials <- trials %>% mutate(
    left  = ifelse(lr_flip, i1, i2),
    right = ifelse(lr_flip, i2, i1)
  )
  # Shuffle trial order
  trials <- trials %>% slice_sample(prop = 1)
  trials$trial_index <- seq_len(nrow(trials))
  trials
}

# ===== UI =====
ui <- fluidPage(
  tags$head(tags$style(HTML("
  .pc-card{
    width:260px; height:320px;              /* ← カードの固定サイズ */
    border:1px solid #ddd; border-radius:12px; padding:12px;
    box-sizing:border-box;
    display:flex; flex-direction:column;
    align-items:center; justify-content:space-between;
    margin:0 auto; text-align:center;
  }
  .pc-img{ width:100%; height:220px;        /* 画像領域の固定サイズ */
           object-fit:contain; display:block; }
  .pc-img-spacer{ width:100%; height:220px; } /* 画像が無い時のスペーサ */
  .pc-label{ margin:8px 0 0; }
"))),
  titlePanel(APP_TITLE),
  fluidRow(
    column(12,
      align = "center",
      textInput("participant_id", "Your ID", value = "Input your ID"),
#      numericInput("seed", "乱数シード (再現用)", value = 2025, step = 1),
#      numericInput("n_reps", "反復回数 (各ペアの提示回数)", value = N_REPS, min = 1, step = 1),
      hr(),
      strong("Description"),
      tags$pre(INSTRUCTION_TEXT),
      hr(),
      uiOutput("status_text"),
    ),
    column(12,
      align="center",
        uiOutput("trial_ui")
    ),
    conditionalPanel(
      align = "center",
      condition = "output.is_finished == true",
      actionButton("send_data", "send the data"),
      br(), br(),
      actionButton("restart", "restart from first")
    )
  )
)

# ===== Server =====
server <- function(input, output, session){
  # Reactive state
  rv <- reactiveValues(
    trials = NULL,
    idx = 0L,
    start_time = NULL,
    records = tibble()
  )
  
  # ---- Helper: finished flag as a reactive (server-side use) ----
  finished <- reactive({
    req(rv$trials)
    rv$idx > nrow(rv$trials)
  })
  
  # Also expose finished to the client for conditionalPanel
  output$is_finished <- reactive({ finished() })
  outputOptions(output, "is_finished", suspendWhenHidden = FALSE)
  
  # ---- Initialize experiment (do not touch input$... directly here) ----
  init_exp <- function(n_reps, seed){
    rv$trials <- make_trials(items_tbl, n_reps = 1, seed = 0)
    rv$idx <- 0L
    rv$records <- tibble()
    go_next()
  }
  
  # Move to next trial
  go_next <- function(){
    rv$idx <- rv$idx + 1L
    rv$start_time <- Sys.time()
  }
  
  # Current trial info
  cur_trial <- reactive({
    req(rv$trials)
    if (rv$idx < 1 || rv$idx > nrow(rv$trials)) return(NULL)
    rv$trials %>% slice(rv$idx)
  })
  
  # Status
  output$status_text <- renderUI({
    req(rv$trials)
    total <- nrow(rv$trials)
    cur <- min(rv$idx, total)
    tagList(
      div(sprintf("progress: %d / %d", cur, total)),
      tags$progress(value = cur, max = total, style = "width:100%")
    )
  })
  
  # Trial UI
  output$trial_ui <- renderUI({
    if (finished()){
      return(tagList(
        h3("The experiment has concluded."),
        p("Prease send the result. Thank you!")
      ))
    }
    tr <- cur_trial(); req(tr)
    left  <- items_tbl %>% filter(item_id == tr$left)
    right <- items_tbl %>% filter(item_id == tr$right)
    
    # Helper to render either image+label or label only
    item_card <- function(item){
      has_img <- !is.na(item$image_url) && nzchar(item$image_url)
      div(class = "pc-card",
          if (has_img) {
            img(src = item$image_url, class = "pc-img")
          } else {
            div(class = "pc-img-spacer")  # 画像なしでも高さを揃える
          },
          div(class = "pc-label", h4(item$label))
      )
    }
      
     
    fluidRow(
      column(6,
             item_card(left),
             div(style = "text-align:center; margin-top:10px;",
                 actionButton("choose_left", paste0("<- This is more ", ATTR_LABEL))
             )
      ),
      column(6,
             item_card(right),
             div(style = "text-align:center; margin-top:10px;",
                 actionButton("choose_right", paste0("This is more ", ATTR_LABEL, " ->"))
             )
      )
    )
  })
  
  # Record a response
  record_choice <- function(choice_id){
    tr <- cur_trial(); req(tr)
    rt <- as.numeric(difftime(Sys.time(), rv$start_time, units = "secs"))
    
    # Which side was chosen
    chosen <- if (choice_id == "left") tr$left else tr$right
    not_chosen <- if (choice_id == "left") tr$right else tr$left
    
    # Save row
    row <- tibble(
      participant_id = input$participant_id,
      trial_index = tr$trial_index,
      rep = tr$rep,
      attr_id = ATTR_ID,           
      attr_label = ATTR_LABEL,     
      item_left = tr$left,
      item_right = tr$right,
      choice = chosen,
      other = not_chosen,
      choice_side = choice_id,
      rt_sec = rt,
      started_at = rv$start_time,
      responded_at = Sys.time()
    )
    rv$records <- bind_rows(rv$records, row)
    
    # Next trial or finish
    if (rv$idx >= nrow(rv$trials)){
      rv$idx <- nrow(rv$trials) + 1L
    } else {
      go_next()
    }
  }
  
  observeEvent(input$choose_left,  ignoreInit = TRUE, { record_choice("left")  })
  observeEvent(input$choose_right, ignoreInit = TRUE, { record_choice("right") })
  observeEvent(input$skip,         ignoreInit = TRUE, { record_choice("skip")  })
  
  # Download data
  observeEvent(input$send_data, {
    req(nrow(rv$records) > 0)
    
    # ファイル名（ID未入力時の保険）
    pid <- if (nzchar(input$participant_id)) input$participant_id else "anon"
    fname <- sprintf("%s_paired_comp_%s_%s.csv", ATTR_LABEL, pid, format(Sys.time(), "%Y%m%d-%H%M%S"))
    
    # 一時CSVを書き出し
    tmpfile <- tempfile(fileext = ".csv")
    readr::write_csv(rv$records, tmpfile)
    
    # 進行表示つきでDriveへアップロード
    withProgress(message = "Sending...", value = 0, {
      incProgress(0.3)
      folder <- googledrive::drive_get(googledrive::as_id(DRIVE_FOLDER_ID))
      incProgress(0.6)
      googledrive::drive_upload(
        media = tmpfile,
        path  = folder,
        name  = fname,
        overwrite = FALSE
      )
      incProgress(1)
    })
    
    showModal(modalDialog(
      title = "Complete sending",
      sprintf("data was successfully sended. Prease close the window：%s", fname),
      easyClose = TRUE
    ))
    
    # 5秒後にセッションを終了（Viewer/ブラウザ問わず有効）
    later::later(function(){
      session$close()    # または 
      shiny::stopApp()
    }, 5)

  })
  
  # Restart button
  observeEvent(input$restart, { init_exp(isolate(input$n_reps), isolate(input$seed)) })
  
  # Re-init when seed or n_reps change
  observeEvent(list(input$seed, input$n_reps), ignoreInit = TRUE, {
    init_exp(input$n_reps, input$seed)
  })
  
  # First init (once) — safely read inputs using isolate
  observeEvent(TRUE, {
    init_exp(isolate(input$n_reps), isolate(input$seed))
  }, once = TRUE)
}

shinyApp(ui, server)
