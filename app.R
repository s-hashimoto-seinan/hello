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

BREAK_BETWEEN_BLOCKS <- TRUE

attrs_tbl <- tibble::tribble(
  ~attr_id, ~label,       ~instruction,
  "A1",     "Attractive",   "Which do you find more `Attractive', the left or the right?",
  "A2",     "Mascular",      "Which do you find more `Muscular', the left or the right?",
  "A3",     "Curious",    "Which do you find more `Curious', the left or the right?",
  "A4",     "Smart",   "Which do you find more `Smart', the left or the right?",
  "A5",     "Perseverance",      "Which do you find more `Perseverance', the left or the right?",
  "A6",     "Courageous",    "Which do you find more `Courageous', the left or the right?",
  "A7",     "Small",   "Which do you find more `Small', the left or the right?",
  "A8",     "Responsible",      "Which do you find more `Responsible', the left or the right?",
  "A9",     "Simple",    "Which do you find more `Simple', the left or the right?",
  "A10",     "Gentle",   "Which do you find more `Gentle', the left or the right?"
)

# 2) Experiment settings
APP_TITLE <- paste0("Paired Comparison Experiment")
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
make_trials <- function(items, attrs, n_reps = 1, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  ids   <- items$item_id
  pairs <- t(combn(ids, 2)) |> as.data.frame() |> setNames(c("i1","i2"))
  
  # attrs の並び順 = ブロック順（必要ならここで sample(attrs$attr_id) に）
  trials_list <- purrr::imap(attrs$attr_id, function(aid, idx){
    row  <- attrs |> dplyr::filter(attr_id == aid) |> dplyr::slice(1)
    reps <- tibble::tibble(rep = seq_len(n_reps))
    reps |>
      tidyr::crossing(pairs) |>
      dplyr::mutate(lr_flip = sample(c(TRUE,FALSE), dplyr::n(), TRUE),
                    left = ifelse(lr_flip, i1, i2),
                    right= ifelse(lr_flip, i2, i1)) |>
      dplyr::select(rep,left,right) |>
      dplyr::slice_sample(prop = 1) |>            # ブロック内だけシャッフル
      dplyr::mutate(attr_id=row$attr_id,
                    attr_label=row$label,
                    instruction=row$instruction,
                    block_index = idx)
  })
  
  trials <- dplyr::bind_rows(trials_list) |> dplyr::arrange(block_index)
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
#     tags$pre(INSTRUCTION_TEXT), 
      uiOutput("instruction_text"),        
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
#      br(), br(),
#      actionButton("restart", "restart from first")
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
    rv$trials  <- make_trials(items_tbl, attrs_tbl, n_reps = 1, seed = 0)
    rv$idx     <- 1L            # 1本目を指しておく
    rv$records <- tibble()
    rv$paused  <- TRUE          # 最初は「ブロック開始待ち」
    rv$start_time <- NULL       # 計測は開始ボタン押下から
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
  
  # Instruction
  output$instruction_text <- renderUI({
    tr <- cur_trial()
    if (is.null(tr)) return(NULL)
    div(HTML(tr$instruction))
  })
  
  # Status
  output$status_text <- renderUI({
    req(rv$trials)
    total <- nrow(rv$trials)
    cur   <- min(rv$idx, total)
    cur_attr <- tryCatch({ cur_trial()$attr_label }, error = function(e) NA)
    tagList(
      div(sprintf("progress: %d / %d", cur, total)),
      tags$progress(value = cur, max = total, style = "width:100%"),
#      if (!is.na(cur_attr)) div(HTML(sprintf("<b>Evaluation Item:</b> %s", cur_attr)))
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
    if (isTRUE(rv$paused)) {
      return(tagList(
        h3(sprintf("Next Evaluation Item: %s", tr$attr_label)),
        p("Press the button below to get started."),
        div(style="text-align:center;",
            actionButton("start_block", "Start this session"))
      ))
    }
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
      
    tagList(
      h3(sprintf("Evaluation Item: %s", tr$attr_label)),
      fluidRow(
        column(6,
               item_card(left),
               div(style = "text-align:center; margin-top:10px;",
                   actionButton("choose_left", paste0("<- This is more ", tr$attr_label))
               )
        ),
        column(6,
               item_card(right),
               div(style = "text-align:center; margin-top:10px;",
                   actionButton("choose_right", paste0("This is more ", tr$attr_label, " ->"))
               )
        )       # ← ここに既存の column(6, ...) ×2 をそのまま
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
      attr_id = tr$attr_id,         # ← 追加
      attr_label = tr$attr_label,   # ← 追加 
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
    
    # 次へ or 終了
    if (rv$idx >= nrow(rv$trials)) {
      rv$idx <- nrow(rv$trials) + 1L
    } else {
      # 次が新しいブロックなら休憩へ、同一ブロックなら即次試行
      next_tr <- rv$trials |> dplyr::slice(rv$idx + 1L)
      cur_block <- tr$block_index
      if (BREAK_BETWEEN_BLOCKS && !is.null(next_tr$block_index) &&
          next_tr$block_index != cur_block) {
        rv$idx <- rv$idx + 1L
        rv$paused <- TRUE
        rv$start_time <- NULL
      } else {
        rv$idx <- rv$idx + 1L
        rv$start_time <- Sys.time()
      }
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
    fname <- sprintf("paired_comp_%s_%s.csv", pid, format(Sys.time(), "%Y%m%d-%H%M%S"))
    
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
  
  # ステップ間フラグ
  rv <- reactiveValues(
    trials = NULL,
    idx = 0L,
    start_time = NULL,
    records = tibble(),
    paused = TRUE   # ← 追加：休憩（開始待ち）状態
  )
  
  # 開始ボタンのハンドラ
  observeEvent(input$start_block, {
    rv$paused <- FALSE
    rv$start_time <- Sys.time()
  })
  
}

shinyApp(ui, server)
