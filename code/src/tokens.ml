
type token =
  | TIMES
  | RIGHTPAR
  | PLUS
  | MINUS
  | LEFTPAR
  | EQUAL
  | EOF
  | DIV
  | COMMA
  | COLON
  | ARROW
  | ID of (Utils.loc * string)
  | LET
  | REC
  | FUN
  | IN
  | UNIT
  | INT
  | BOOL
  | TYID of string
  | UNITV of Utils.loc
  | NUM of (Utils.loc * int)
  | TRUE of Utils.loc
  | FALSE of Utils.loc
  | FST
  | SND
  | IF
  | THEN
  | ELSE
  | AND
  | OR
  | GT
