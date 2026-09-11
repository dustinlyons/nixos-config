-- Addons cannot write files. The game client can: /chatlog appends every chat
-- line it displays to Logs/WoWChatLog.txt, but the switch resets each session.
-- This flips it back on at login so wow-chat-watch always has a file to tail.
local f = CreateFrame("Frame")
f:RegisterEvent("PLAYER_LOGIN")
f:SetScript("OnEvent", function()
  if not LoggingChat() then
    LoggingChat(true)
  end
end)
