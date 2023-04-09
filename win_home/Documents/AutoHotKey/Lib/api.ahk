;--------------------------------------------------------------------------------
; Win32API
; https://morakana.hatenadiary.org/entry/20090702/1246526840
;--------------------------------------------------------------------------------
api_AttachThreadInput(AttachFrom, AttachTo, Flag)
{
    Return DllCall("user32.dll\AttachThreadInput"
        , "UInt", AttachFrom
        , "UInt", AttachTo
        , "Int", Flag
        , "Int")
}

api_CharLower(pString)
{
    Return DllCall("user32.dll\CharLower"
        , "UInt", pString
        , "UInt")
}

api_CharUpper(pString)
{
    Return DllCall("user32.dll\CharUpper"
        , "UInt", pString
        , "UInt")
}

api_CloseHandle(Handle)
{
    Return DllCall("kernel32.dll\CloseHandle"
        , "UInt", Handle
        , "Int")
}

api_CreateFile(pFileName, Access, Share, Security, Disposition, Attributes, hTemplate)
{
    Return DllCall("kernel32.dll\CreateFile"
        , "UInt", pFileName
        , "UInt", Access
        , "UInt", Share
        , "UInt", Security
        , "UInt", Disposition
        , "UInt", Attributes
        , "UInt", hTemplate
        , "UInt")
}

api_CreateToolhelp32Snapshot(Flags, th32ProcessID)
{
        Return DllCall("kernel32.dll\CreateToolhelp32Snapshot"
        , "UInt", Flags
        , "UInt", th32ProcessID
        , "UInt")
}

api_FormatMessage(Flag, pSource, MessageId, LanguageId, pBuffer, BufferSize, Arguments)
{
    Return DllCall("kernel32.dll\FormatMessage"
        , "UInt", Flag
        , "UInt", pSource
        , "UInt", MessageId
        , "UInt", LanguageId
        , "UInt", pBuffer
        , "UInt", BufferSize
        , "UInt", Arguments
        , "UInt")
}

api_GetClassName(hWindow, pBuffer, BufferSize)
{
    Return DllCall("user32.dll\GetClassName"
        , "UInt", hWindow
        , "UInt", pBuffer
        , "Int", BufferSize
        , "Int")
}

api_GetCurrentDirectory(BufferSize, pBuffer)
{
    Return DllCall("kernel32.dll\GetCurrentDirectory"
        , "UInt", BufferSize
        , "UInt", pBuffer
        , "UInt")
}

api_GetCurrentThreadId()
{
    Return DllCall("kernel32.dll\GetCurrentThreadId"
        , "UInt")
}

api_GetCursorPos(pPoint)
{
    Return DllCall("user32.dll\GetCursorPos"
        , "UInt", pPoint
        , "Int")
}

api_GetDriveType(pRootDirectoryName)
{
    Return DllCall("kernel32.dll\GetDriveType"
        , "UInt", pRootDirectoryName
        , "UInt")
}

api_GetFileSizeEx(hFile, pLargeInteger)
{
    Return DllCall("kernel32.dll\GetFileSizeEx"
        , "UInt", hFile
        , "UInt", pLargeInteger
        , "Int")
}

api_GetForegroundWindow()
{
    Return DllCall("user32.dll\GetForegroundWindow"
        , "UInt")
}

api_GetFullPathName(pFileName, BufferSize, pBuffer, pFilePart)
{
    Return DllCall("kernel32.dll\GetFullPathName"
        , "UInt", pFileName
        , "UInt", BufferSize
        , "UInt", pBuffer
        , "UInt", pFilePart
        , "UInt")
}

api_GetLastError()
{
    Return DllCall("kernel32.dll\GetLastError"
        , "UInt")
}

api_GetLongPathName(pFileName, pBuffer, BufferSize)
{
    Return DllCall("kernel32.dll\GetLongPathName"
        , "UInt", pFileName
        , "UInt", pBuffer
        , "UInt", BufferSize
        , "UInt")
}

api_GetShortPathName(pFileName, pBuffer, BufferSize)
{
    Return DllCall("kernel32.dll\GetShortPathName"
        , "UInt", pFileName
        , "UInt", pBuffer
        , "UInt", BufferSize
        , "UInt")
}

api_GetTickCount()
{
    Return DllCall("kernel32.dll\GetTickCount"
        , "UInt")
}

api_GetWindowLong(hWindow, nIndex)
{
    Return DllCall("user32.dll\GetWindowLong"
        , "UInt", hWindow
        , "Int", nIndex
        , "Int")
}

api_GetWindowText(hWindow, pBuffer, BufferSize)
{
    Return DllCall("user32.dll\GetWindowText"
        , "UInt", hWindow
        , "UInt", pBuffer
        , "UInt", BufferSize
        , "Int")
}

api_GetWindowThreadProcessId(hWindow, pPID)
{
    Return DllCall("user32.dll\GetWindowThreadProcessId"
        , "UInt", hWindow
        , "UInt", pPID
        , "UInt")
}

api_ImmGetDefaultIMEWnd(hWindow)
{
    Return DllCall("imm32.dll\ImmGetDefaultIMEWnd"
        , "Uint", hWindow
        , "UInt")
}

api_IsWindow(hWindow)
{
    Return DllCall("user32.dll\IsWindow"
        , "UInt", hWindow
        , "Int")
}

api_lstrcpy(pStringTo, pStringFrom)
{
    Return DllCall("kernel32.dll\lstrcpy"
        , "UInt", pStringTo
        , "UInt", pStringFrom
        , "UInt")
}

api_lstrcpyn(pStringTo, pStringFrom, MaxLength)
{
    Return DllCall("kernel32.dll\lstrcpyn"
        , "UInt", pStringTo
        , "UInt", pStringFrom
        , "Int", MaxLength
        , "UInt")
}

api_mciSendString(pCommandString, pBuffer, BufferSize, hCBWindow)
{
    Return DllCall("winmm.dll\mciSendStringA"
        , "UInt", pCommandString
        , "UInt", pBuffer
        , "UInt", BufferSize
        , "UInt", hCBWindow
        , "UInt")
}

api_mciGetErrorString(ErrorCode, pBuffer, BufferSize)
{
    Return DllCall("winmm.dll\mciGetErrorStringA"
        , "UInt", ErrorCode
        , "UInt", pBuffer
        , "UInt", BufferSize
        , "Int")
}

api_PathFileExists(pFileName)
{
    Return DllCall("shlwapi.dll\PathFileExistsA"
        , "UInt", pFileName)
}

api_PathFindExtension(pFileName)
{
    Return DllCall("shlwapi.dll\PathFindExtensionA"
        , "UInt", pFileName
        , "UInt")
}

api_PathFindFileName(pFileName)
{
    Return DllCall("shlwapi.dll\PathFindFileNameA"
        , "UInt", pFileName
        , "UInt")
}

api_PathGetDriveNumber(pFileName)
{
    Return DllCall("shlwapi.dll\PathGetDriveNumberA"
        , "UInt", pFileName
        , "Int")
}

api_PathIsDirectory(pFileName)
{
    Return DllCall("shlwapi.dll\PathIsDirectoryA"
        , "UInt", pFileName
        , "Int")
}

api_PathRemoveExtension(pFileName)
{
    Return DllCall("shlwapi.dll\PathRemoveExtensionA"
        , "UInt", pFileName
        , "UInt")
}

api_PathRemoveFileSpec(pFileName)
{
    Return DllCall("shlwapi.dll\PathRemoveFileSpecA"
        , "UInt", pFileName
        , "Int")
}

api_Process32First(hSnapshot, pPROCESSENTRY32)
{
    Return DllCall("kernel32.dll\Process32First"
        , "UInt", hSnapshot
        , "UInt", pPROCESSENTRY32
        , "Int")
}

api_Process32Next(hSnapshot, pPROCESSENTRY32)
{
    Return DllCall("kernel32.dll\Process32Next"
            , "UInt", hSnapshot
            , "UInt", pPROCESSENTRY32
            , "Int")
}

api_ReadFile(hFile, pBuffer, ReadSize, &ReadedSize, pOverlapped)
{
    Return DllCall("kernel32.dll\ReadFile"
        , "UInt", hFile
        , "UInt", pBuffer
        , "UInt", ReadSize
        , "UIntP", ReadedSize
        , "UInt", pOverlapped
        , "Int")
}

api_SendMessage(hWindow, Message, wParam, lParam)
{
    Return DllCall("user32.dll\SendMessage"
        , "UInt", hWindow
        , "UInt", Message
        , "Int", wParam
        , "Int", lParam
        , "Int")
}

api_ShellExecute(hWindow, pVerb, pFileName, pParameters, pDirectory, ShowCmd)
{
    Return DllCall("shell32.dll\ShellExecuteA"
        , "UInt", hWindow
        , "UInt", pVerb
        , "UInt", pFileName
        , "UInt", pParameters
        , "UInt", pDirectory
        , "Int", ShowCmd
        , "UInt")
}

api_timeBeginPeriod(uPeriod)
{
    Return DllCall("winmm.dll\timeBeginPeriod"
        , "UInt", uPeriod
        , "UInt")
}

api_timeEndPeriod(uPeriod)
{
    Return DllCall("winmm.dll\timeEndPeriod"
        , "UInt", uPeriod
        , "UInt")
}

api_timeGetTime()
{
    Return DllCall("winmm.dll\timeGetTime"
        , "UInt")
}

api_TurnOffMonitor()
{
    SC_MONITORPOWER := 0xF170
    WM_SYSCOMMAND := 0x0112
    MONITOR_OFF := 2
    Return SendMessage(WM_SYSCOMMAND, SC_MONITORPOWER, MONITOR_OFF,, "Program Manager")
}

api_WindowFromPoint(xpos, ypos)
{
    Return DllCall("user32.dll\WindowFromPoint"
        , "Int", xpos
        , "Int", ypos
        , "UInt")
}

api_WriteFile(hFile, pBuffer, WriteSize, &WrittenSize, pOverlapped)
{
    Return DllCall("kernel32.dll\WriteFile"
        , "UInt", hFile
        , "UInt", pBuffer
        , "UInt", WriteSize
        , "UIntP", WrittenSize
        , "UInt", pOverlapped
        , "Int")
}
