PROCEDURE GetEnvValue:
    DEFINE INPUT  PARAMETER envKey    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER envValue  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE envFile   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE line      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE key       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE valueLine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.

    ASSIGN
        envFile  = "src/.env"
        envValue = "NOT_FOUND".

    INPUT FROM VALUE(envFile).
    REPEAT:
        IMPORT UNFORMATTED line.
        
        IF INDEX(line, "=") > 0 THEN DO:
            ASSIGN
                idx       = INDEX(line, "=")
                key       = TRIM(SUBSTRING(line, 1, idx - 1))
                valueLine = TRIM(SUBSTRING(line, idx + 1)).
            
            IF key = envKey THEN DO:
                ASSIGN envValue = valueLine.
                LEAVE.
            END.
        END.
    END.
    INPUT CLOSE.
END PROCEDURE.


