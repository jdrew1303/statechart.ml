type event_type = External | Internal | Platform

type t = {
  name: string;
  event_type: event_type;
  sendid: string;
  origin: string;
  origintype: string;
  invokeid: string;
  (* data: TODO *)
}
