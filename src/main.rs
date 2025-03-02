//! `lvis` (`lsof` visualization) is a tool for creating a graph visualization of the `lsof` command output.
//! The `lsof` command is used to list open files (in this unix sense where "everything" is a file)
//! and the processes that opened them.
//!
//! It can be used to view which processes are accessing a particular file, which
//! ports are used for network connections, identifiers for ends of pipes and socket connections
//! among other things. Given files can be shared between processes, this makes it interesting to plot
//! for visualizing to uncover relationships for poking around and debugging.
//!
//! The tool is a GUI which highlights relationships (edges) between processes and files (nodes), metadata
//! about the components, and can be used to show the files each process has and visa versa. It also adds
//! additional logic to infer some other relationships between files, such as bidirectional connections
//! between pipes, sockets, and network connections.

use std::env;

use anyhow::Result;
use runner::start_app;

mod lsof;
mod runner;
mod visualization;

/// Entrypoint for the `lvis` application.
fn main() -> Result<()> {
    env_logger::init();
    let args: Vec<String> = env::args().skip(1).collect();
    start_app(args)
}
