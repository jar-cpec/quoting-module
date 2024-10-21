USING quote.services.*.

DEF VAR quotingService AS CLASS QuotingService NO-UNDO.
quotingService = NEW QuotingService() NO-ERROR.

quotingService:CreateQuote() NO-ERROR.

quotingService:GetQuote("123") NO-ERROR.