use rand::prelude::*;
use std::env;
use std::sync::mpsc::*;
use std::thread;
use std::thread::*;

fn mk_tasks(n: usize) -> Vec<usize> {
    let size = n * (n + 1) / 2;
    let mut v = Vec::with_capacity(size);

    for i in 0..n {
        let times = n - i;
        v.extend([i + 1].into_iter().cycle().take(times));
    }

    v
}

enum Msg {
    Calc(usize),
    Quit,
}

struct Worker {
    handle: JoinHandle<()>,
    chan: Sender<Msg>,
}

fn fib(n: usize) -> usize {
    if n < 2 {
        1
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

impl Worker {
    pub fn new(num: usize) -> Worker {
        let (send, recv) = channel();

        let handle = thread::spawn(move || {
	    eprintln!("Worker {num} started");

            let mut done = 0;
            while let Ok(msg) = recv.recv() {
                match msg {
                    Msg::Calc(n) => {
                        fib(n);
                        done = done + n
                    }
                    Msg::Quit => {
                        eprintln!("Worker {num} done {done} tasks");
                        break;
                    }
                }
            }
        });

        Worker { handle, chan: send }
    }

    pub fn send(&self, msg: Msg) {
        self.chan.send(msg).expect("Failed to send a msg")
    }

    pub fn wait(self) {
        self.handle.join().expect("Failed to join")
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let default_volume = 10;
    let volume = match args.get(1) {
        None => default_volume,
        Some(v) => v.parse::<usize>().unwrap_or(default_volume),
    };
    let tasks = mk_tasks(volume);
    let num_workers = 4;
    let workers: Vec<_> = (0..num_workers).map(|n| Worker::new(n)).collect();

    let mut rng = thread_rng();
    for t in tasks {
        let wn = rng.gen_range(0..num_workers);
        let w = &workers[wn];
        w.send(Msg::Calc(t))
    }

    for w in &workers {
        w.send(Msg::Quit)
    }
    for w in workers {
        w.wait()
    }

    println!("Main done");
}
