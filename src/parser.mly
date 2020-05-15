%{

%}

%token <string> LINE
%token <string> SURVEY_TITLE
%token <string> SECTION_TITLE
%token <string> QUESTION
%token FREE_INPUT
%token <string> EXCLUSIVE_CHOICE
%token <int> LIMIT
%token OTHER
%token EOF
%token SEP

%start survey
%type <Survey.t> survey


%%
survey:
    name = SURVEY_TITLE; list (txt); sections = nonempty_list(section); EOF {
        Survey.{ name; sections }
    }
;

section:
    title = SECTION_TITLE; list(txt); questions = nonempty_list (question); {
        Survey.{ title; questions }
    }
;

question:
    text = QUESTION; FREE_INPUT; option (SEP)   {  Survey.{ kind = FreeInput; text; choices=[]}}
|   text = QUESTION; l = option(LIMIT); choices = nonempty_list(QUESTION); o=option(OTHER); SEP {
    let limit = match l with None -> -1 | Some i -> i in
    let other = match o with None -> false | Some _ -> true in
    Survey.{ kind = Checkbox(other, limit); text; choices }
}
|   text = QUESTION; option(LIMIT); choices = nonempty_list (EXCLUSIVE_CHOICE); option(SEP);{
    Survey.{ kind = Multiple; text; choices }
}
;
txt:
    LINE  { () }
    | SEP  { () }
    ;