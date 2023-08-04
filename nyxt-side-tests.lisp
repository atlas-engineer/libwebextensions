(nyxt:ffi-web-extension-send-message
 (nyxt:current-buffer)
 (webkit:webkit-user-message-new "addExtension" (glib:g-variant-new-string "{\"name\":\"test\"}"))
 nil nil)

(nyxt:ffi-buffer-evaluate-javascript (nyxt:current-buffer) "browser.test.prop" "test")

(nyxt:ffi-buffer-evaluate-javascript (nyxt:current-buffer) "browser.test.prop2" "test")

(nyxt:ffi-buffer-evaluate-javascript (nyxt:current-buffer)
                                     "browser.test.method(null).then((v) => {browser.test.prop2 = v; return null;})" "test")
