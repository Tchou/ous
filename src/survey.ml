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

let choices_to_js l =
  let open Format in
  printf "  item.setChoiceValues([\n   %a]);\n"
  (pp_print_list ~pp_sep:(fun f () -> fprintf f ",\n   ")
  (fun f s -> fprintf f "%S" s)) l

let question_to_js q =
  let open Format in
  match q.kind with
    Multiple ->
      printf "  item = form.addMultipleChoiceItem();\n" ;
      printf "  item.setTitle(%S);\n" q.text;
(*      printf "  item.setRequired(true);"; *)
      choices_to_js q.choices;
  | Checkbox (free_form, limit) ->
      printf "  item = form.addCheckboxItem();\n";
      printf "  item.setTitle(%S);" q.text;
(*      printf "  item.setRequired(true);"; *)
      choices_to_js q.choices;
      if free_form then
        printf "  item.showOtherOption(true);\n";
      if limit > 0 then begin
        printf "  item.setHelpText(\"Select at most %d choices.\");\n" limit;
        printf "  item.setValidation(FormApp.createCheckboxValidation().requireSelectAtMost(%d).setHelpText(\"Select at most %d choices.\").build());\n"
        limit limit;
      end
  | FreeInput ->
      printf "  item = form.addParagraphTextItem();\n";
      printf "  item.setTitle(%S);\n" q.text
let section_to_js first s =
  let open Format in
    printf "  /*** SECTION %s ***/\n" s.title;
    if first then
      printf "  form.addSectionHeaderItem().setTitle(%S);\n" s.title
    else
      printf "  form.addPageBreakItem().setTitle(%S);\n" s.title;
    List.iter question_to_js s.questions;
    printf "\n"

let to_js t =
  let open Format in
  printf "function init () {\n";
  printf "  let form = null;\n";
  printf "  let section = null;\n";
  printf "  let item = null;\n";
  (* use the line below to target a specific form *)
  (* printf "  form = FormApp.openById('1K....');\n"; *)
  printf "  form = FormApp.getActiveForm();\n";
  printf "  form.getItems().forEach(function (i) { form.deleteItem(i);});\n";
  printf "  form.setProgressBar(true);\n";
  printf "  form.setTitle(%S);\n\n" t.name;
  List.iteri (fun i s -> section_to_js (i=0) s) t.sections;
  printf "}\n"

