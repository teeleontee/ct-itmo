#![forbid(unsafe_code)]

use std::io;
use std::net::{TcpListener, TcpStream};
use std::thread;

fn run(address: &str, destination: String) -> io::Result<()> {
    let tcp_listener = TcpListener::bind(address)?;
    let mut threads = vec![];

    for incoming in tcp_listener.incoming() {
        // nonblocking
        let mut client = incoming?;
        let mut prox = TcpStream::connect(destination.clone())?;
        let mut cloned_client = client.try_clone()?;
        let mut cloned_prox = prox.try_clone()?;
        threads.push(thread::spawn(move || {
            let _ = io::copy(&mut client, &mut prox);
        }));
        threads.push(thread::spawn(move || {
            let _ = io::copy(&mut cloned_prox, &mut cloned_client);
        }));
    }

    for t in threads {
        t.join().unwrap();
    }

    Ok(())
}

pub fn run_proxy(port: u32, destination: String) {
    let address = format!("127.0.0.1:{}", port);
    let res = run(&address, destination);
    match res {
        Ok(_) => {}
        Err(error) => {
            println!("oh no... {}", error);
        }
    }
}
