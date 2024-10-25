DEFINE VARIABLE logFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE currentDate AS CHARACTER NO-UNDO.
DEFINE VARIABLE timestamp AS CHARACTER NO-UNDO.
DEFINE VARIABLE logLine AS CHARACTER NO-UNDO.

PROCEDURE WriteToLog:
    DEF INPUT PARAMETER inLevel AS CHARACTER NO-UNDO.
    DEF INPUT PARAMETER inMessage AS CHARACTER NO-UNDO.
    
    IF inLevel <> "INFO" AND inLevel <> "WARN" AND inLevel <> "ERROR" THEN DO:
        MESSAGE "Invalid log level, default to INFO" VIEW-AS ALERT-BOX ERROR.
        inLevel = "INFO".
    END.

    currentDate = STRING(TODAY, "99-99-9999").
    logFile = SUBSTITUTE("C:\Code\quoting-module\src\logs\&1.log", currentDate).

    FILE-INFO:FILE-NAME = STRING(logFile).

    timestamp = STRING(TIME, "HH:MM:SS").
    
    logLine = "[" + STRING(timestamp) + "]" + " " + "[" + STRING(inLevel) + "]" + " " + STRING(inMessage).

    OUTPUT TO VALUE(logFile) APPEND.
    PUT UNFORMATTED logLine SKIP. 
    OUTPUT CLOSE.

END PROCEDURE.