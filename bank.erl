-module(bank).
-export([listen/1, grant_money/5]).

listen(BankEts) ->
	receive
		{startBank} ->
			timer:sleep(100),
			listen(BankEts);
		{requestAmount, Sender, Name, Amount, RequestedBank} ->
			grant_money(Sender, Name, BankEts, Amount, RequestedBank),
			listen(BankEts)
	end.

grant_money(Sender, Name, BankEts, Amount, RequestedBank) ->
	[[AvailableAmount, BankPID]] = ets:match(BankEts, {RequestedBank,'$1', '$2'}),
	if 
		AvailableAmount >= Amount ->
			AmountLeft = AvailableAmount - Amount,
			ets:insert(BankEts, {RequestedBank, AmountLeft, BankPID}),
			Sender ! {amountGranted, Name, Amount, RequestedBank};
		true ->
			Sender ! {amountRejected, Name, Amount, RequestedBank}
	end.