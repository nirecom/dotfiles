; ForceJapaneseLayout.ahk (AutoHotkey v2)
; Forces keyboard layout back to Japanese (0x0411) if it changes

#Requires AutoHotkey v2.0
Persistent

SetTimer(CheckLayout, 500)

CheckLayout() {
    try {
        hwnd := WinGetID("A")
        threadId := DllCall("GetWindowThreadProcessId", "UInt", hwnd, "UInt", 0)
        hkl := DllCall("GetKeyboardLayout", "UInt", threadId, "UPtr")
        langId := hkl & 0xFFFF

        if (langId != 0x0411) {  ; Not Japanese
            PostMessage(0x50, 0, 0x04110411,, "A")  ; WM_INPUTLANGCHANGEREQUEST
        }
    }
}
