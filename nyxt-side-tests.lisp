(nyxt:ffi-web-extension-send-message
 (nyxt:current-buffer)
 (webkit:webkit-user-message-new "addExtension" (glib:g-variant-new-string "{\"name\":\"borderify\"}"))
 nil nil)

(nyxt:ffi-buffer-evaluate-javascript
 (nyxt:current-buffer) "browser instanceof Object" "borderify")

(nyxt:ffi-buffer-evaluate-javascript
 (nyxt:current-buffer) "browser.tabs.TAB_ID_NONE" "borderify")

(nyxt:ffi-buffer-evaluate-javascript
 (nyxt:current-buffer) "browser.tabs.create({\"url\": \"about:blank\"})" "borderify")
