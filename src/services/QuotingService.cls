CLASS src.services.QuotingService:

    DEFINE STATIC VARIABLE hLogger AS HANDLE NO-UNDO.

    CONSTRUCTOR STATIC QuotingService():
        RUN src/utils/logger.p PERSISTENT SET hLogger.
    END CONSTRUCTOR.

    METHOD PUBLIC VOID CreateQuote():
        RUN WriteToLog IN hLogger ("INFO", "MESSAGE FROM CREATE-QUOTE METHOD").
        
        
        
        
        
        
        RETURN.
    END METHOD.

    METHOD PUBLIC VOID UpdateQuote():
        RUN WriteToLog IN hLogger ("INFO", "MESSAGE FROM UPDATE-QUOTE METHOD").
        RETURN.
    END METHOD.

    METHOD PUBLIC VOID DeleteQuote():
        RUN WriteToLog IN hLogger ("INFO", "MESSAGE FROM DELETE-QUOTE METHOD").
        RETURN.
    END METHOD.

    METHOD PUBLIC VOID GetQuote(INPUT quoteId AS CHARACTER):
        RUN WriteToLog IN hLogger ("ERROR", "MESSAGE FROM GETQUOTE METHOD").
        RETURN.
    END METHOD.

    METHOD PUBLIC VOID GetQuotes():
        RUN WriteToLog IN hLogger ("INFO", "MESSAGE FROM GETQUOTES METHOD").
        RETURN.
    END METHOD.

END CLASS.
