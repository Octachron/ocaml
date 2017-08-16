type 'a t = < m:unit; .. > as 'a (* the constraint seems necessary *)
type s = <m:unit>
let r = object method m = () end
