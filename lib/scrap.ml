open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Soup
(* open Base *)

let uri =
  Uri.of_string
    "https://www.google.com/search?q=rtx%203060&source=lnms&tbm=shop&sa=X&ved=2ahUKEwiC-5PKneD5AhXLR7gEHY6nCnAQ_AUoAnoECAIQBA&biw=2560&bih=854&dpr=1.5"

let headers =
  Header.add_list (Header.init ())
    [
      ("authority", "www.google.com");
      ( "accept",
        "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9"
      );
      ("accept-language", "en-US,en;q=0.9");
      ("cache-control", "max-age=0");
      ("referer", "https://www.google.com/");
      ("sec-ch-ua-mobile", "?0");
      ("sec-ch-ua-wow64", "?0");
      ("sec-fetch-dest", "document");
      ("sec-fetch-mode", "navigate");
      ("sec-fetch-site", "same-origin");
      ("sec-fetch-user", "?1");
      ("upgrade-insecure-requests", "1");
      ( "user-agent",
        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like \
         Gecko) Chrome/103.0.0.0 Safari/537.36" );
      ( "x-client-data",
        "CKy1yQEIj7bJAQiitskBCKmdygEI7f3KAQiUocsBCNvvywEIssHMAQjFwcwBCNfBzAEI18bMAQidycwBCOPLzAEYq6nKAQ=="
      );
    ]

let sopa =
  Client.call ~headers `GET uri
  >>= (fun (_res, body_stream) ->
        body_stream |> Cohttp_lwt.Body.to_string >|= Soup.parse)
  |> Lwt_main.run

let fetch_page url =
  Client.call ~headers `GET (Uri.of_string url) >>= fun (_res, body_stream) ->
  body_stream |> Cohttp_lwt.Body.to_string >|= Soup.parse

let page = sopa $ "#rso > div ~ div"
let print_list = List.map print_string

type raw_offer = { price : string; name : string; seller : string }

let titles = sopa $$ "h4" |> iter (fun a -> print_endline (R.leaf_text a))
let links = sopa $ "#rso > div ~ div" $$ "a[target]"
let link_element = links |> to_list |> fun x -> List.nth x 1
let remove_nondigit = Str.global_replace (Str.regexp "[^0-9]+") ""

type offer = { title : string; price : float; link : string; seller : string }
[@@deriving show]

let sort_offers offer_a offer_b = offer_a.price -. offer_b.price |> int_of_float

let _ =
  print_string @@ show_offer { title = ""; price = 0.0; link = ""; seller = "" }

module OfferMap = Map.Make (String)

let parent_exn el = parent el |> Option.get

let make_search_term =
  Printf.sprintf
    "https://www.google.com/search?q=%s&source=lnms&tbm=shop&sa=X&ved=2ahUKEwiC-5PKneD5AhXLR7gEHY6nCnAQ_AUoAnoECAIQBA&biw=2560&bih=854&dpr=1.5"

let snd (_, b) = b

let scrap_offers page =
  page
  $$ "a[target]"
  |> to_list
  |> List.fold_left
       (fun acc link_element ->
         try
           let container =
             try
               link_element
               |> parent_exn
               |> parent_exn
               |> parent_exn
               |> parent_exn
             with _ -> failwith "Failed to find link container"
           in

           let link = R.attribute "href" link_element in
           let seller = R.leaf_text (link_element $ "div + div + div") in
           let title = R.leaf_text (container $ "h4") in

           let price =
             container
             $ "span > span > span > span"
             |> R.leaf_text
             |> remove_nondigit
             |> float_of_string
             |> fun n -> n /. 100.
           in

           OfferMap.add title { link; title; seller; price } acc
         with _ -> acc)
       OfferMap.empty
  |> OfferMap.bindings
  |> List.map snd
  |> List.sort sort_offers
(* let l = r |> OfferMap.bindings |> List.map (fun (_, o) -> show_offer o) *)

let products =
  sopa
  $$ "span > a > div + div + div"
  |> iter (fun a ->
         parent a
         |> Option.get
         |> trimmed_texts
         |> String.concat "\n\n"
         |> print_endline)
