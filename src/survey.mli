type question_kind =
Multiple (* exclusive choice *)
| Checkbox of bool * int (* multiple choice, has free form, limit *)
| FreeInput


type question = {
kind : question_kind;
text : string;
choices : string list;
}

type section = {
title : string;
questions : question list
}

type t = {
name : string;
sections : section list;
}
val to_js : t -> unit
