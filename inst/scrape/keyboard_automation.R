automate_table_download <- function(clicks1, clicks2, filename) {
  chrome_path <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
  cmd <- sprintf('"%s" "https://mferac.gov.si/4bjf/sl"', chrome_path)
  system(cmd, wait = FALSE)
  Sys.sleep(6)

  # Build PowerShell command
  ps_cmd <- paste0('powershell -command "
    Add-Type -AssemblyName System.Windows.Forms
    Add-Type -AssemblyName Microsoft.VisualBasic

    # Find Chrome process
    $chrome = Get-Process | Where-Object {$_.ProcessName -eq \\\"chrome\\\" -and $_.MainWindowTitle -ne \\\"\\\"}
    if ($chrome) {
      [Microsoft.VisualBasic.Interaction]::AppActivate($chrome[0].Id)
      Start-Sleep -Milliseconds 1000

      # Step 1: Navigate to selected table and click
      for($i=1; $i -le ', clicks1, '; $i++) {
        [System.Windows.Forms.SendKeys]::SendWait(\\\"{TAB}\\\")
        Start-Sleep -Milliseconds 200
      }
      [System.Windows.Forms.SendKeys]::SendWait(\\\"{ENTER}\\\")
      Start-Sleep -Milliseconds 2000

      # Step 2: Navigate to download icon and click
      for($i=1; $i -le ', clicks2, '; $i++) {
        [System.Windows.Forms.SendKeys]::SendWait(\\\"{TAB}\\\")
        Start-Sleep -Milliseconds 100
      }
      [System.Windows.Forms.SendKeys]::SendWait(\\\"{ENTER}\\\")
      Start-Sleep -Milliseconds 2000

      # Step 3: Navigate to correct table
      for($i=1; $i -le 3; $i++) {
        [System.Windows.Forms.SendKeys]::SendWait(\\\"{TAB}\\\")
        Start-Sleep -Milliseconds 100
      }

      # Step 4: Access three-dot menu
      [System.Windows.Forms.SendKeys]::SendWait(\\\"%+{F10}\\\")
      Start-Sleep -Milliseconds 300
      [System.Windows.Forms.SendKeys]::SendWait(\\\"{RIGHT}\\\")
      Start-Sleep -Milliseconds 100
      [System.Windows.Forms.SendKeys]::SendWait(\\\"{RIGHT}\\\")
      Start-Sleep -Milliseconds 100
      [System.Windows.Forms.SendKeys]::SendWait(\\\"{ENTER}\\\")
      Start-Sleep -Milliseconds 100

      # Step 5: Select Export Data and Export
      [System.Windows.Forms.SendKeys]::SendWait(\\\"{ENTER}\\\")
      Start-Sleep -Milliseconds 1000

      # Step 6: Navigate in export dialog
      for($i=1; $i -le 3; $i++) {
        [System.Windows.Forms.SendKeys]::SendWait(\\\"{TAB}\\\")
        Start-Sleep -Milliseconds 200
      }
      [System.Windows.Forms.SendKeys]::SendWait(\\\"{ENTER}\\\")
      Start-Sleep -Milliseconds 4000

      # Step 7: Type filename in Save As dialog
      [System.Windows.Forms.SendKeys]::SendWait(\\\"',
      paste(filename, Sys.Date(), sep = "_"), '\\\")
      Start-Sleep -Milliseconds 500
      [System.Windows.Forms.SendKeys]::SendWait(\\\"{ENTER}\\\")
    }
  "')
  system(ps_cmd)
}

close_chrome <- function() {
  system('powershell -command "Get-Process | Where-Object {$_.ProcessName -eq \\\"chrome\\\"} | Stop-Process -Force"')
}

# single table
automate_table_download(12, 2, "test_DP")

tables_config <- data.frame(
  name = c("DP", "KBJF", "OB", "ZZZS", "ZPIZ"),
  clicks1 = c(12, 14, 15, 16, 17),
  clicks2 = c(2, 1, 1, 1, 1),
  stringsAsFactors = FALSE
)

# run over all the tables
purrr::pwalk(tables_config, function(name, clicks1, clicks2) {
  automate_table_download(clicks1, clicks2, name)
})
close_chrome()
