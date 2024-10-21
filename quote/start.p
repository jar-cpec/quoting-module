DEF VAR quotingService AS CLASS quote.services.QuotingService NO-UNDO.
quotingService = NEW quote.services.QuotingService() NO-ERROR.

quotingService:CreateQuote() NO-ERROR.

quotingService:GetQuote("123") NO-ERROR.

