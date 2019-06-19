-module(money).
-export([start/0, create_customers/3, create_banks/2]).

create_customers([H|Tail], CustomerEts, BankList) ->
	{Name, Amount} = H,
	io:fwrite("~s: ~w~n", [Name, Amount]),
	PID = spawn(customer, listen, [CustomerEts]),
	ets:insert(CustomerEts, {Name, Amount, PID, BankList}),
	PID ! {request, self(), Name},
	create_customers(Tail, CustomerEts, BankList);
create_customers([], CustomerEts, BankList) -> 
	io:fwrite("~n~n~n"),
	ok.

create_banks([H|Tail], BankEts) ->
	{Name, Amount} = H,
	io:fwrite("~s: ~w~n", [Name, Amount]),
	PID = spawn(bank, listen, [BankEts]),
	ets:insert(BankEts, {Name, Amount, PID}),
	PID ! {startBank},
	create_banks(Tail, BankEts);
create_banks([], BankEts) ->
	ok.

listen(BankEts, CustomerEts) ->
	receive
		{customerRequest, Name} ->
			request_amount(Name, BankEts, CustomerEts),
			listen(BankEts, CustomerEts);
		{amountGranted, Name, Amount, RequestedBank} ->
			send_amount_granted(CustomerEts, Name, Amount, RequestedBank),
			listen(BankEts, CustomerEts);
		{amountRejected, Name, Amount, RequestedBank} ->
			send_amount_rejected(CustomerEts, Name, Amount, RequestedBank),
			listen(BankEts, CustomerEts);
		{customerComplete, CustomerPID, Name} ->
			kill_customer(CustomerPID, Name),
			listen(BankEts, CustomerEts);
		{customerNoBank, CustomerPID, Name} ->
			kill_customer_no_bank(CustomerPID, Name, CustomerEts),
			listen(BankEts, CustomerEts)
	after 2500 -> 
		kill_main_process(BankEts)
	end.

get_bank_list([H|Tail], Result) ->
	{Name, Amount} = H,
	get_bank_list(Tail, lists:append(Result, [Name]));
get_bank_list([], Result) ->
	Result.
	
start() ->
	% MasterPID = spawn(money, start_banking, []),
	% keep_main_runnining(MasterPID).
	{_, Customers} = file:consult("./customers.txt"),
	{_, Banks} = file:consult("./banks.txt"),
	io:fwrite("~n~n** Banks and financial resources **~n"),
	BankEts = ets:new(bankTable,[ordered_set, public]),
	create_banks(Banks, BankEts),
	CustomerEts = ets:new(customerTable,[ordered_set, public]),
	io:fwrite("~n~n~n** Customers and loan objectives **~n"),
	create_customers(Customers, CustomerEts, get_bank_list(Banks, [])),
	listen(BankEts, CustomerEts).

% keep_main_runnining(MasterPID) ->
% 	MasterAlive = is_process_alive(MasterPID),
% 	if
% 		MasterAlive ->
% 			keep_main_runnining(MasterPID);
% 		true ->
% 			ok
% 	end.

get_random_bank(BankEts, BankList) ->
	Bank = lists:nth(rand:uniform(length(BankList)), BankList),
	[[_, PID]] = ets:match(BankEts, {Bank,'$1', '$2'}),
	{Bank, PID}.

request_amount(Name, BankEts, CustomerEts) ->
	[[AmountRequired, _, BankList]] = ets:match(CustomerEts, {Name,'$1', '$2', '$3'}),
	timer:sleep(10 * rand:uniform(10)),
	if
		AmountRequired =< 50 ->
			Amount = rand:uniform(AmountRequired);
		true ->
			Amount = rand:uniform(50)
	end,
	{RequestedBank, PID} = get_random_bank(BankEts, BankList),
	io:fwrite("~s requests a loan of ~w from ~p ~n", [Name, Amount, RequestedBank]),
	PID ! {requestAmount, self(), Name, Amount, RequestedBank}.

send_amount_granted(CustomerEts, Name, Amount, RequestedBank) ->
	io:fwrite("~s granted loan of ~p to ~p ~n", [RequestedBank, Amount, Name]),
	[[_, CustomerPID, _]] = ets:match(CustomerEts, {Name,'$1', '$2', '$3'}),
	CustomerPID ! {moneyGranted, self(), Amount}.

send_amount_rejected(CustomerEts, Name, Amount, RequestedBank) ->
	io:fwrite("~s rejected loan of ~p to ~p ~n", [RequestedBank, Amount, Name]),
	[[_, CustomerPID, _]] = ets:match(CustomerEts, {Name,'$1', '$2', '$3'}),
	CustomerPID ! {moneyRejected, self(), RequestedBank}.

kill_customer(CustomerPID, Name) ->
	io:fwrite("~s Objective Completed ~n", [Name]),
	exit(CustomerPID, "Objective Completed").

kill_customer_no_bank(CustomerPID, Name, CustomerEts) ->
	[[AmountLeft, _, _]] = ets:match(CustomerEts, {Name,'$1', '$2', '$3'}),
	io:fwrite("~s has no bank left to request money. Money left: ~w~n", [Name, AmountLeft]),
	exit(CustomerPID, "No bank left").

show_bank_info([H|T]) ->
	{Bank, AmountLeft, _} = H,
	io:fwrite("Bank: ~s has money left: ~w~n", [Bank, AmountLeft]),
	show_bank_info(T);
show_bank_info([]) ->
	ok.

kill_main_process(BankEts) ->
	io:format("~n~n~nMaster received no replies for 2.5 seconds, ending...~n~n~n"),
	BankList = ets:tab2list(BankEts),
	show_bank_info(BankList).


% start_banking() ->
% 	{_, Customers} = file:consult("./customers.txt"),
% 	{_, Banks} = file:consult("./banks.txt"),
% 	io:fwrite("~n~n** Banks and financial resources **~n"),
% 	BankEts = ets:new(bankTable,[ordered_set, public]),
% 	create_banks(Banks, BankEts),
% 	CustomerEts = ets:new(customerTable,[ordered_set, public]),
% 	io:fwrite("~n~n~n** Customers and loan objectives **~n"),
% 	create_customers(Customers, CustomerEts, get_bank_list(Banks, [])),
% 	listen(BankEts, CustomerEts).