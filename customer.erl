-module(customer).
-export([listen/1]).

listen(CustomerEts) ->
	receive
		{request, Sender, Name} ->
			timer:sleep(100),
			request_money(Sender, Name, CustomerEts),
			listen(CustomerEts);
		{moneyGranted, Sender, Amount} ->
			reduce_amount(CustomerEts, Sender, Amount),
			listen(CustomerEts);
		{moneyRejected, Sender, RequestedBank} ->
			remove_bank(CustomerEts, Sender, RequestedBank),
			listen(CustomerEts)
	end.

reduce_amount(CustomerEts, Sender, Amount) ->
	[[Name, CurrentAmount, BankList]] = ets:match(CustomerEts, {'$1','$2', self(), '$3'}),
	AmountLeft = CurrentAmount - Amount,
	ets:insert(CustomerEts, {Name, AmountLeft, self(), BankList}),
	request_money(Sender, Name, CustomerEts).

remove_bank(CustomerEts, Sender, RequestedBank) ->
	[[Name, Amount, BankList]] = ets:match(CustomerEts, {'$1','$2', self(), '$3'}),
	ets:insert(CustomerEts, {Name, Amount, self(), lists:delete(RequestedBank, BankList)}),
	if
		length(BankList) =< 1 ->
			Sender ! {customerNoBank, self(), Name};
		true ->
			request_money(Sender, Name, CustomerEts)
	end.

request_money(Sender, Name, CustomerEts) ->
	[[AmountLeft, _]] = ets:match(CustomerEts, {Name,'$1', self(), '$2'}),
	if
		AmountLeft =< 0 ->
			Sender ! {customerComplete, self(), Name};
		true ->
			Sender ! {customerRequest, Name}
	end.