//! Runs the `lsof` command and streams the output to a channel.
//!
//! Also parses the output into a [`LsofEntry`] struct.
//! For `lsof` programmatic format semantics, see:
//! <https://www.man7.org/linux/man-pages/man8/lsof.8.html#OUTPUT_FOR_OTHER_PROGRAMS>

use std::collections::HashMap;
use std::ffi::CString;
use std::io::{BufRead, BufReader};
use std::ops::Deref;
use std::os::fd::{AsRawFd, FromRawFd, IntoRawFd};
use std::sync::mpsc::{self, Receiver};
use std::thread::{self, JoinHandle};

use nix::libc::{STDOUT_FILENO, c_char, execvp};
use nix::pty::{OpenptyResult, Winsize, openpty};
use nix::unistd::{ForkResult, Pid, close, dup2, fork};
use thiserror::Error;

/// Like the `lsof` man page states, this
/// specifies a character list, f, that selects the fields to
/// be output for processing by another program, and the
/// character that terminates each output field. `lvis` uses only a subset of the fields.
/// See: <https://www.man7.org/linux/man-pages/man8/lsof.8.html#OUTPUT_FOR_OTHER_PROGRAMS>
const CHARACTER_LIST: &str = "cpndtfsLikPT";

/// Represents a line of output from `lsof` in the programmatic format
pub struct LsofOutputLine(String);

impl Deref for LsofOutputLine {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Error)]
pub enum LsofError {
    #[error("Missing expected field: {0}")]
    MissingExpectedField(String),
}

/// Presents one file resource of a particular process in `lsof` programmatic format
#[derive(Debug, Clone, Default)]
pub struct LsofEntry {
    // c    process command name (all characters from proc or user structure)
    // Should always be present
    pub command: Option<String>,
    // p    process ID (always selected)
    // Should always be present
    pub pid: Option<String>,
    // n    file name, comment, Internet address
    pub file_name: Option<String>,
    // d    file's device character code
    pub device: Option<String>,
    // t    file's type
    pub file_type: Option<String>,
    // Details that are typically displayed in `lsof`
    // s    file's size (decimal)
    pub size: Option<String>,
    // L    process login name
    pub user: Option<String>,
    // f    file descriptor
    pub fd: Option<String>,
    // i    file's inode number
    pub inode: Option<String>,
    // k    link count
    pub link_count: Option<String>,
    // P    protocol
    pub protocol: Option<String>,
    // tcp specific fields
    pub tcp: Option<LsofTcpEntry>,
    // Custom parsed connections, useful for types such as PIPE and TCP which have implicit connections
    // with other file descriptors/ports
    pub connections: Option<FileConnection>,
}

impl LsofEntry {
    /// Returns process details for the process node, each element is a line in the detail output
    pub fn process_details(&self) -> Result<Vec<String>, LsofError> {
        let mut details = vec![];
        details.push(format!(
            "Command: {}",
            self.command
                .as_ref()
                .ok_or(LsofError::MissingExpectedField("command".to_string()))?
        ));
        details.push(format!(
            "PID: {}",
            self.pid
                .as_ref()
                .ok_or(LsofError::MissingExpectedField("pid".to_string()))?
        ));
        if let Some(user) = &self.user {
            details.push(format!("User: {}", user));
        }
        Ok(details)
    }

    /// Returns details for the file node, each element is a line in the detail output
    pub fn file_details(&self) -> Result<Vec<String>, LsofError> {
        let mut details = vec![];
        details.push(format!(
            "File Name: {}",
            self.file_name
                .as_ref()
                .ok_or(LsofError::MissingExpectedField("file_name".to_string()))?
        ));
        if let Some(device) = &self.device {
            details.push(format!("Device: {}", device));
        }
        if let Some(file_type) = &self.file_type {
            details.push(format!("File Type: {}", file_type));
        }
        if let Some(size) = &self.size {
            details.push(format!("Size: {}", size));
        }
        if let Some(inode) = &self.inode {
            details.push(format!("Inode: {}", inode));
        }
        if let Some(link_count) = &self.link_count {
            details.push(format!("Link Count: {}", link_count));
        }
        if let Some(protocol) = &self.protocol {
            details.push(format!("Protocol: {}", protocol));
        }
        if let Some(tcp) = &self.tcp {
            if let Some(connection_state) = &tcp.connection_state {
                details.push(format!("Connection State: {}", connection_state));
            }
            if let Some(read_queue_size) = &tcp.read_queue_size {
                details.push(format!("Read Queue Size: {}", read_queue_size));
            }
            if let Some(send_queue_size) = &tcp.send_queue_size {
                details.push(format!("Send Queue Size: {}", send_queue_size));
            }
        }
        Ok(details)
    }
}

/// Presents TCP specific fields in `lsof` programmatic format for additional details
#[derive(Debug, Clone, Default)]
pub struct LsofTcpEntry {
    // TQR=<read queue size>
    pub read_queue_size: Option<String>,
    // TQS=<send queue size>
    pub send_queue_size: Option<String>,
    // TST=<connection state>
    pub connection_state: Option<String>,
}

/// Represents a connection between two "files" (e.g. a PIPE or TCP connection)
/// If this field is present, then the process node will have an edge to the source file,
/// the source file will connect to the dest file, and the dest file likely will have
/// an edge to its corresponding process node (if the dest process is running on the local machine)
#[derive(Debug, Clone)]
pub struct FileConnection {
    pub source: Option<String>,
    pub dest: String,
}

impl FileConnection {
    /// Parses a file name to extract the source and dest file names, if present
    /// Used for entries such as PIPEs, unix sockets, and TCP connections using ports
    /// ```
    /// command      123 user     1      PIPE 0xf96595c4eb146f93        16384                     ->0x958b10a08c1d8a1c
    /// command2     456 user     3u     unix 0x54d349a788e96b42          0t0                     ->0xcd4c1c579836dbc9
    /// command3     665 user     90u    IPv4 0x34e027a6bfbce435          0t0  TCP                  localhost:hbci->localhost:59981 (CLOSED)
    /// ```
    pub fn parse_from_file_name(file_name: &str) -> Option<FileConnection> {
        // If there is a `->` in the file name, split on it. The first part (if present) is the source and the second is the dest
        let parts: Vec<&str> = file_name.splitn(2, "->").collect();
        if parts.len() == 2 {
            let source = match parts[0].trim() {
                "" => None,
                s => Some(s.to_string()),
            };
            Some(FileConnection {
                source,
                dest: parts[1].trim().to_string(),
            })
        } else if parts.len() == 1 {
            // If there is no `->`, then the file name is the source and the dest is the same as the source
            Some(FileConnection {
                source: None,
                dest: parts[0].trim().to_string(),
            })
        } else {
            None
        }
    }
}

/// Contains mappings between the source and dest file identifiers for [`FileConnection`]s
/// Used to create implicit edges between files (e.g. PIPEs, unix sockets, and TCP connections using ports)
#[derive(Debug, Default)]
pub struct FileConnectionMap {
    // key is the source file name, value is the list of file node ids that have the source file as a dest
    // (source file can be dest because some connections are not only bidirectional but also share the same identifiers
    // for send and receive like PIPEs. While TCP is bidirectional different ephemeral ports are used for send and receive)
    // These are not process nodes because each file node will first map to another file node, which will then have a corresponding
    // process node
    pub source_to_file_nodes: HashMap<String, Vec<usize>>,
    // key is the dest file name, value is the list of file node ids that have the dest file as a source
    // both the forward and reverse mappings are needed because the order in which we process source and dest nodes
    // is not guaranteed. If we process a `src -> dest` entry but the `dest` node is not created yet, then it seems
    // there is no edge. This edge would be created later using this dest node mapping.
    // e.g. `localhost:8080 -> localhost:8081` implies an edge between the file nodes for `localhost:8080` and `localhost:8081`.
    // If we see the file node for `localhost:8081` later in the sequence, we would need to wait to create the edge.
    pub dest_to_file_nodes: HashMap<String, Vec<usize>>,
}

// Could return multiple entries because one pid can have multiple files open
// This is a stream of `LsofEntry`s. This basically parses the stream of input lines
// into a stream of `LsofEntry`s
// General structure is:
// ```
// pid
//  ... pid specific fields
//  file descriptor
//  ... file specific fields
//  file descriptor
//  ... file specific fields
//  [repeats]
// pid
// [repeats]
// ```
// We want to emit an entry for each (pid, file descriptor)
pub fn stream_from_receiver(rx: Receiver<LsofOutputLine>) -> Receiver<LsofEntry> {
    let (tx_entry, rx_entry) = mpsc::channel();

    thread::spawn(move || {
        // The pid and command can be shared across multiple entries, per the lsof programmatic format spec
        let mut has_seen_pid = false;
        let mut has_seen_file_name = false;
        let mut lsof_entry: LsofEntry = LsofEntry::default();
        while let Ok(line) = rx.recv() {
            // Parse the line according to the programmatic format
            // The first character is the field type, the rest is the field value
            let field_type = line.chars().next().unwrap();
            if !has_seen_pid && field_type != 'p' {
                // this is unexpected, arguably could panic
                eprintln!(
                    "Expected pid field as first field, got {}. Will skip until pid is seen.",
                    field_type
                );
                continue;
            }
            let field_value = line[1..].to_string();
            match field_type {
                'c' => lsof_entry.command = Some(field_value),
                'p' => {
                    if has_seen_pid {
                        if let Err(e) = tx_entry.send(lsof_entry.clone()) {
                            eprintln!("Error sending LsofEntry: {}", e);
                        }
                    }
                    lsof_entry = LsofEntry::default();
                    lsof_entry.pid = Some(field_value);
                    // file info is per pid
                    has_seen_file_name = false;
                    has_seen_pid = true;
                }
                'n' => {
                    // heuristic file connection parsing
                    if let Some(mut connection) = FileConnection::parse_from_file_name(&field_value)
                    {
                        if lsof_entry.device.is_some() && connection.source.is_none() {
                            connection.source = Some(lsof_entry.device.clone().unwrap());
                        }
                        lsof_entry.connections = Some(connection);
                    }
                    // some file names have `->` as a prefix, remove this
                    lsof_entry.file_name = Some(field_value.trim_start_matches("->").to_string());
                }
                'd' => lsof_entry.device = Some(field_value),
                't' => lsof_entry.file_type = Some(field_value),
                // f    file descriptor
                'f' => {
                    // Indicates the previous file name entry is complete
                    if has_seen_file_name {
                        if let Err(e) = tx_entry.send(lsof_entry.clone()) {
                            eprintln!("Error sending LsofEntry: {}", e);
                        }
                    }
                    has_seen_file_name = true;
                    lsof_entry.fd = Some(field_value);
                }
                's' => lsof_entry.size = Some(field_value),
                'L' => lsof_entry.user = Some(field_value),
                'i' => lsof_entry.inode = Some(field_value),
                'k' => lsof_entry.link_count = Some(field_value),
                'P' => lsof_entry.protocol = Some(field_value),
                'T' => {
                    // Entry of the form
                    // TST=LISTEN
                    // TQR=0
                    // TQS=0
                    let parts: Vec<&str> = field_value.splitn(2, '=').collect();
                    if parts.len() == 2 {
                        let key = parts[0];
                        let value: &str = parts[1];
                        if lsof_entry.tcp.is_none() {
                            lsof_entry.tcp = Some(LsofTcpEntry::default());
                        }
                        match key {
                            "ST" => {
                                lsof_entry.tcp.as_mut().unwrap().connection_state =
                                    Some(value.to_string())
                            }
                            "QR" => {
                                lsof_entry.tcp.as_mut().unwrap().read_queue_size =
                                    Some(value.to_string())
                            }
                            "QS" => {
                                lsof_entry.tcp.as_mut().unwrap().send_queue_size =
                                    Some(value.to_string())
                            }
                            _ => {}
                        }
                    }
                }
                _ => {
                    // ignore other fields
                }
            }
        }
        // send the last entry
        tx_entry.send(lsof_entry).unwrap();
    });

    rx_entry
}

/// Runs `lsof` and streams the output to a channel.
/// Instead of just spawning a child process directly, this runs `lsof`
/// in a pseudo terminal to trick `lsof` into not bufferring the output
/// If you try to run `lsof` where the output file descriptor is not a tty (to a pipe for example),
/// it will buffer causing long delays any data is returned. not very interactive!
pub fn run_lsof(args: &[String]) -> (mpsc::Receiver<LsofOutputLine>, JoinHandle<()>, Pid) {
    let (tx, rx) = mpsc::channel();

    // the window size doesn't matter since we're not using the pty interactively
    let winsize = Winsize {
        ws_row: 24,
        ws_col: 80,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };

    // prefer openpty over forkpty because we can explicitly control which of stdout and stderr is forwarded from slave to master
    // forkpty will default forward both, but we don't want to parse error messages as lsof entries. They should be directly sent to terminal
    let OpenptyResult { master, slave } = openpty(Some(&winsize), None).expect("Failed to openpty");
    match unsafe { fork() } {
        Ok(ForkResult::Parent { child }) => {
            close(slave.as_raw_fd()).expect("Failed to close slave");
            // must use `into_raw_fd` to take ownership of the fd from master, otherwise the fd will be
            // closed on `master` drop which makes it invalid
            let master_reader = unsafe { std::fs::File::from_raw_fd(master.into_raw_fd()) };
            // slave should be closed in child
            std::mem::forget(slave);
            let handle = thread::spawn(move || {
                let reader = BufReader::new(master_reader);
                for line in reader.lines() {
                    match line {
                        Ok(l) => {
                            log::trace!("Line from lsof: {}", l);
                            tx.send(LsofOutputLine(l)).unwrap();
                        }
                        Err(e) => eprintln!("Error reading line: {}", e),
                    }
                }
            });
            (rx, handle, child)
        }
        Ok(ForkResult::Child) => {
            close(master.as_raw_fd()).expect("Failed to close master");
            // master should be closed in parent
            std::mem::forget(master);
            // stdout is written to the pty slave (and to the master)
            dup2(slave.into_raw_fd(), STDOUT_FILENO).expect("Failed to dup2 stdout in slave");
            // stderr is directly inherited by the parent process (like stderr(Stdio::inhert()))

            let lsof = CString::new("lsof").unwrap();
            let mut exec_args = vec![lsof.clone()];
            // use programmatic format displaying all fields by default
            exec_args.push(CString::new("-F").unwrap());
            exec_args.push(CString::new(CHARACTER_LIST).unwrap());
            for arg in args {
                exec_args.push(CString::new(arg.as_str()).unwrap());
            }
            let mut ptrs: Vec<*const c_char> = exec_args.iter().map(|arg| arg.as_ptr()).collect();
            ptrs.push(std::ptr::null());
            unsafe { execvp(lsof.as_ptr(), ptrs.as_ptr()) };
            unreachable!();
        }
        Err(e) => {
            eprintln!("Failed to fork errno: {}", e);
            std::process::exit(1);
        }
    }
}

/// Check if the user passed a help flag to `lsof`
pub fn is_help(args: &[String]) -> bool {
    args.iter().any(|arg| arg == "-h" || arg == "--help")
}
