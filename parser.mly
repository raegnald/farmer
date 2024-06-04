%{
%}

%token<string> FILEPATH
%token COLON EOF

%start<Association.t list> assocs

%%

assocs:
  | xs = list(assoc); EOF { xs }

assoc:
  | target = FILEPATH; COLON+; link = FILEPATH
    { { target; link } }
