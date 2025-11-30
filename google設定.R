install.packages(c("googledrive"))  # 未導入なら
library(googledrive)

# 使うスコープは最小権限に（アップロードなら drive.file でOK）
drive_auth(
  email  = "s-hashimoto@seinan-gakuin.jp",
  scopes = "https://www.googleapis.com/auth/drive",
  cache  = ".secrets"        # ← ここにトークンを保存
)

# 認証後、動くか1回だけテスト
drive_user()                 # 自分のアカウントが出ればOK

gargle::gargle_oauth_sitrep()

list.files(all.files = TRUE) 

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "s-hashimoto@seinan-gakuin.jp"
)


googledrive::drive_user() 

id <- googledrive::as_id("1Vu0mGWF7EDFpUh1Zt3lIUdwu_HBJ_rQw")
googledrive::drive_get(id)      # 見える？（エラーなら権限/ID）
googledrive::drive_ls(id, n_max = 1) 


# 1) 今のアカウント
drive_user()

FOLDER_ID <-"1Vu0mGWF7EDFpUh1Zt3lIUdwu_HBJ_rQw" 

# 2) IDの形（空白やクエリを除去）
id <- trimws("1Vu0mGWF7EDFpUh1Zt3lIUdwu_HBJ_rQw")   # folders/ の後ろの部分のみ

# 3) そのIDが見えるか？
drive_get(as_id("1Vu0mGWF7EDFpUh1Zt3lIUdwu_HBJ_rQw"))          # ← ここがNGなら「ID違い」か「権限不足」

# 4) 見えたら中身を1件だけ列挙（共有ドライブでもOK）
drive_ls(as_id(id), n_max = 1)

library(readr)

dest <- as_id(FOLDER_ID)
drive_get(dest)     

sink("test.txt")
print("test")
sink()

drive_upload(media = "test.txt", path = dest, type = "text/csv", overwrite = TRUE)

rsconnect::writeManifest()
