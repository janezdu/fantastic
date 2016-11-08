open model  
open Controller
open Server

val translate_to_json: diff -> json
val translate_to_diff: json -> diff
val send_json: json -> unit
