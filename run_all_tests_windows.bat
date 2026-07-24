@echo off
setlocal EnableExtensions EnableDelayedExpansion

cd /d "%~dp0"

where sbt.bat >nul 2>&1
if errorlevel 1 (
  echo Error: sbt.bat was not found in PATH. 1>&2
  exit /b 127
)

set "LOG_FILE=output.log"
set "FAILED=0"
set "TESTS=ataxno gemmno k2mmno k3mmno fftno mno kmpno nwno spmvno"

for %%T in (%TESTS%) do (
  echo.
  echo ========== Running %%T ==========

  call sbt.bat -mem 12384 -batch -no-colors "runMain mycgratemporal.Main %%T" >"%LOG_FILE%" 2>&1
  set "TEST_STATUS=!ERRORLEVEL!"

  if not "!TEST_STATUS!"=="0" (
    echo Test process exited with status !TEST_STATUS!. Parsing its log before continuing. 1>&2
    set "FAILED=1"
  )

  echo ---------- Parsing %LOG_FILE% ----------
  call sbt.bat -mem 12384 -batch -no-colors "runMain OutParse.Main %LOG_FILE%"
  set "PARSE_STATUS=!ERRORLEVEL!"

  if not "!PARSE_STATUS!"=="0" (
    echo Log parser exited with status !PARSE_STATUS!. 1>&2
    set "FAILED=1"
  )
)

echo.
echo ========== Test sequence finished ==========

if "!FAILED!"=="1" (
  echo One or more test or parser processes failed. 1>&2
  exit /b 1
)

echo All test processes and log parser runs completed successfully.
exit /b 0
