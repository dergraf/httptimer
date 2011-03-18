-record(httptimer, {
	id, 		%% Timer Id  
	tref,		%% Erlang Timer Reference
	time,		%% Execution Time (in ms from now()) 
	status,		%% Execution Status
	created_on, 	%% Creation Timestamp
	url,    	%% Url to request
	headers,	%% Http Headers to send 
	method, 	%% Http Method to use
	body		%% Request Body
}).
