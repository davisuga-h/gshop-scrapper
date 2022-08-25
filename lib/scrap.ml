
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Soup



let uri = Uri.of_string "https://www.google.com/search?q=rtx%203060&source=lnms&tbm=shop&sa=X&ved=2ahUKEwiC-5PKneD5AhXLR7gEHY6nCnAQ_AUoAnoECAIQBA&biw=2560&bih=854&dpr=1.5" 
let headers = Header.add_list (Header.init ()) [
  ("cookie", "SID=NghNJfz9neIG_CUnxyPYQgG3hmb7v-TsbvuvbwLCdxDP_xPSH3tRqMjMRcFCVy7bWGBclg.; __Secure-1PSID=NghNJfz9neIG_CUnxyPYQgG3hmb7v-TsbvuvbwLCdxDP_xPSVRw-WU9Ioee-CUH_mCEYaQ.; __Secure-3PSID=NghNJfz9neIG_CUnxyPYQgG3hmb7v-TsbvuvbwLCdxDP_xPS_KZJ6rHtB5ZdHYBygt2rXA.; HSID=AJw5ho5eNuC48lI2B; SSID=AplEBv1yGntsPLmeb; APISID=amX7HpjIf9YcPE6F/AfF3xb3TPo945GM9F; SAPISID=DNfO6KOzMF5Vavrf/AEXgi9JBQ8SYfcYWI; __Secure-1PAPISID=DNfO6KOzMF5Vavrf/AEXgi9JBQ8SYfcYWI; __Secure-3PAPISID=DNfO6KOzMF5Vavrf/AEXgi9JBQ8SYfcYWI; AEC=AakniGNnxe4zwqYEU55nn5mfINCJsonbkKMyFhXTD8hb4lQQ1fsDArBSaw; OTZ=6650987_68_64_73560_68_416340; NID=511=UhgWNGN0ynjI4kXXF4USVAGCd5Nfay9ksmSww9AKvUkxCYzkdWAA486BXE1Xv5PrzvKnpOdwNSx-OPrNwjgLuEo4TPYQELDw_-Rl6HTUmW6BlfukbE2QD2FqcUwxWW9sd7l_0PtT9QlCeRqHPJDDYSNwha8yCdxl6Q_DDxcOxQANqW50feHRKUn7JOXDKSF6kzb82LR33aCAbnVjNcvmX0XvClADOoY7klMXi8R5_iFgi-uieZDL2jjxVLA; DV=8zWOnto_7P4UkFjbmuSQit74x-gVLRg; UULE=a+cm9sZTogMQpwcm9kdWNlcjogMTIKdGltZXN0YW1wOiAxNjYxMzcwMDExNDc1MDAwCmxhdGxuZyB7CiAgbGF0aXR1ZGVfZTc6IC0xOTgzMTQ5ODAKICBsb25naXR1ZGVfZTc6IC00Mzk2MzY2ODIKfQpyYWRpdXM6IDEyNDAwCnByb3ZlbmFuY2U6IDYK; 1P_JAR=2022-08-24-19; SIDCC=AEf-XMQ51K26ds5I91hKJOgUARmIKQ5pLpzNbHMvMZWbThTcI_IlY3ZVGnSt0-LWFpntjOKLhQ; __Secure-1PSIDCC=AEf-XMQxlyCLG1IW2WvPkH0cwnyug0lW72qkPOsP8eVCz1MQMmGwHmAVcIaIkIQsDMDzq7SeUoQ; __Secure-3PSIDCC=AEf-XMSwYq3hjdiTMC5vPzricDNn7ojHjWBoVzPNDEFf3jn2p8R-aBJEWpU7Ocw24f5xMjy51Rs");
  ("authority", "www.google.com");
  ("accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9");
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
  ("user-agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36");
  ("x-client-data", "CKy1yQEIj7bJAQiitskBCKmdygEI7f3KAQiUocsBCNvvywEIssHMAQjFwcwBCNfBzAEI18bMAQidycwBCOPLzAEYq6nKAQ==");
] 

let sopa = ((Client.call ~headers `GET uri
>>= fun (res, body_stream) ->
  body_stream |> Cohttp_lwt.Body.to_string  >|= Soup.parse) |> Lwt_main.run) 

let print_list = List.map print_string

type raw_offer = {price: string; name: string; seller: string}

let titles = sopa $$ "h4"
|> iter (fun a -> print_endline (R.leaf_text a))

let products = sopa $$ "span > a > div + div + div"
|> iter (fun a -> parent a|> Option.get |> trimmed_texts  |> String.concat "\n\n" |> print_endline);
