let file = (Yojson.Basic.from_file ("worlds/"^file_name)) in
let init_state_var = init_state file in
