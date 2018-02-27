# bonny
Ship your Racket web-server like a true pirate!

## Story

Anne Bonny was one of the few women to engage in piracy during the 18th century, golden age of pirates.  
She lived a adventurous and risky life on the sea, fighting and plundering ships.  
  
I needed a name for my "web container shipping manager", so, as a tribute, I'll make her the captain of my web-server ship.

## Goal

This is an attempt at writing the simplest container management tool ever, using the **Racket web-server** to handle webhooks, **systemd-nspawn** for containers, **machinectl** to check their status, and an optional **nginx** server to reverse-proxy and load-balance requests.  

## Usage

`raco pkg install command-tree` (the only project requirement)  
`./make install`  (setup the racket environment)  
`./make test` (check that everything is alright)  
`./make run` (run the bonny server)  

## Production setup

When you're at ease with the way **bonny** works, you can simply setup a systemd unit:  
`cp ./bonny.service /usr/lib/systemd/system/bonny.service`  
`systemctl enable bonny`  
`systemctl start bonny`  
