-- Addons cannot write files. The game client can: /chatlog appends every chat
-- line it displays to Logs/WoWChatLog.txt. Two catches this addon works around:
--   * the switch resets each session, so it is turned on at login;
--   * the client buffers the file and only writes it out when the file is
--     closed (a whole 4-minute session landed on disk in one write at exit),
--     so the log is closed and reopened every few seconds to force the flush.
-- Neither LoggingChat() call prints anything: the "Logging chat to ..." line
-- comes from the /chatlog slash command, not from the API.
local FLUSH_SECONDS = 5

local function flush()
  if LoggingChat() then
    LoggingChat(false)
    LoggingChat(true)
  end
end

local f = CreateFrame("Frame")
f:RegisterEvent("PLAYER_LOGIN")
f:SetScript("OnEvent", function()
  LoggingChat(true)
  C_Timer.NewTicker(FLUSH_SECONDS, flush)
end)
