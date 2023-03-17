module Error_report = Log.New_scheme ()


type t = Warnings.loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type report_kind =
  | Report_error
  | Report_warning of string
  | Report_warning_as_error of string
  | Report_alert of string
  | Report_alert_as_error of string


type _ Log.extension +=
  | Report_kind: report_kind extension
  | Main_location: location extension
  | Sub_location: location extension


let scheme = Error_report.init ()

let report_kind = function
  | Report_error -> "error",   _
  | Report_warning s -> "warning", s
  | Report_alert s -> "alert", s
  | Report_warning_as_error s -> "warning_as_error", s
  | Report_alert_as_error s -> "alert_as_error", s


let kind = Log.new_key ~path:["kind"] scheme
    Log.(
      Custom { default = Pair (String, String); pull = report_kind; id = Report_kind }
    )
