USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Quote:
    
    DEFINE PUBLIC PROPERTY QuoteId AS CHARACTER NO-UNDO 
        GET.
        SET. 
        
    DEFINE PUBLIC PROPERTY QuoteDate AS DATE NO-UNDO 
        GET.
        SET. 
        
    DEFINE PUBLIC PROPERTY DealerName AS CHARACTER NO-UNDO 
        GET.
        SET. 

    CONSTRUCTOR PUBLIC Quote (bQuoteId AS CHARACTER, bQuoteDate AS DATE, bDealerName AS CHARACTER):
        SUPER ().
        
        ASSIGN 
            THIS-OBJECT:QuoteId = bQuoteId.        
        THIS-OBJECT:QuoteDate = bQuoteDate.
        THIS-OBJECT:DealerName = bDealerName.
               
    END CONSTRUCTOR.

END CLASS.