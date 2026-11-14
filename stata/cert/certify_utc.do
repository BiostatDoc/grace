version 19
clear all
local stata_dir : environment GRACEDR_STATA_DIR
assert !missing("`stata_dir'")
adopath ++ "`stata_dir'/ado"

local system_root : environment SYSTEMROOT
local launcher "`system_root'/System32/WindowsPowerShell/v1.0/powershell.exe"
local flags "-NoProfile -Command"
local offset_command "[TimeZoneInfo]::Local.GetUtcOffset([DateTime]::Now).TotalMinutes"
local redirect = char(62)
tempfile offset_file
quietly shell "`launcher'" `flags' "`offset_command'" `redirect' "`offset_file'"
tempname handle
file open `handle' using "`offset_file'", read text
file read `handle' reading
file close `handle'
local offset_minutes = real(strtrim(`"`reading'"'))
assert !missing(`offset_minutes')

_gracedr_utc
local utc = r(time)
local local_now = clock("`c(current_date)' `c(current_time)'", "DMYhms")
local difference = `utc' - `local_now'
local expected = (0 - `offset_minutes') * 60 * 1000
assert abs(`difference' - `expected') < 120000

_gracedr_utc
local later = r(time)
assert inrange(`later', `utc', `utc' + 60000)

display "certify_utc: all checks passed"
