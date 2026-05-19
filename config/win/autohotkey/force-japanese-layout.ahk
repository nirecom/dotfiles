; force-japanese-layout.ahk (AutoHotkey v2)
; Forces keyboard layout back to Japanese (0x0411) if it changes

#Requires AutoHotkey v2.0
Persistent

; RegisterShellHookWindow delivers WM_SHELLHOOK to this script's message loop.
; The message number is dynamic — resolved via RegisterWindowMessage.
WM_SHELLHOOK := DllCall("RegisterWindowMessage", "Str", "SHELLHOOK", "UInt")

if (WM_SHELLHOOK = 0) {
    OutputDebug("force-japanese-layout: RegisterWindowMessage failed — timer-only mode")
} else {
    if !DllCall("RegisterShellHookWindow", "Ptr", A_ScriptHwnd, "Int") {
        OutputDebug("force-japanese-layout: RegisterShellHookWindow failed (error "
            . A_LastError . ") — fallback timer only")
    }
    OnMessage(WM_SHELLHOOK, OnShellHook)
}

CheckLayout()  ; Correct any non-Japanese layout active at startup

; Low-frequency safety net: covers startup races and delivery gaps.
SetTimer(CheckLayout, 15000)

OnShellHook(wParam, lParam, msg, hwndScript) {
    static HSHELL_WINDOWACTIVATED := 0x0004
    static HSHELL_LANGUAGE        := 0x0008

    event := wParam & 0xFFFF
    if (event = HSHELL_WINDOWACTIVATED) {
        CheckLayoutForHwnd(lParam)  ; lParam is the activated HWND
    } else if (event = HSHELL_LANGUAGE) {
        CheckLayout()  ; lParam encodes language info, not a target HWND
    }
}

CheckLayoutForHwnd(hwnd) {
    try {
        threadId := DllCall("GetWindowThreadProcessId", "Ptr", hwnd, "Ptr", 0)
        hkl := DllCall("GetKeyboardLayout", "UInt", threadId, "UPtr")
        langId := hkl & 0xFFFF
        if (langId != 0x0411) {  ; Not Japanese
            PostMessage(0x50, 0, 0x04110411,, "A")  ; WM_INPUTLANGCHANGEREQUEST
        }
    }
}

CheckLayout() {
    try {
        hwnd := WinGetID("A")
        CheckLayoutForHwnd(hwnd)
    }
}
