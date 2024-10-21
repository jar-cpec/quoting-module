USING quote.services.* FROM PROPATH.

DEF VAR quotingService AS QuotingService NO-UNDO.
quotingService = NEW QuotingService() NO-ERROR.

quotingService:CreateQuote() NO-ERROR.

quotingService:GetQuote("123") NO-ERROR.