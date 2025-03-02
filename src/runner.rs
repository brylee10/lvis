//! Starts the `lvis` application

use std::{
    process::exit,
    thread::{self, JoinHandle},
};

use anyhow::{Result, anyhow};
use eframe::{NativeOptions, run_native};
use egui::{Vec2, ViewportBuilder};
use nix::sys::wait::{WaitStatus, waitpid};

use crate::{
    lsof::{is_help, run_lsof},
    visualization::{LsofVis, VIEWPORT_HEIGHT, VIEWPORT_WIDTH},
};

/// Starts the `lvis` application
pub fn start_app(args: Vec<String>) -> Result<()> {
    let (lsof_output_rx, lsof_handle, child) = run_lsof(&args);

    let child_handle: JoinHandle<Result<()>> = thread::spawn(move || {
        // wait for lsof to exit
        //log::debug!("waiting for lsof to exit");
        // if error, terminates process, which also stops the GUI
        loop {
            let status = match waitpid(child, None) {
                Err(e) => {
                    // unwrap: the corresponding receiver should not be dropped yet
                    // exit code 1 is default convention for an error, `waitpid` error otherwise does not have a
                    // clear corresponding exit code
                    eprintln!("lsof child threw an error: {:?}", e);
                    exit(1);
                }
                Ok(status) => status,
            };
            match status {
                WaitStatus::Exited(_, code) => {
                    if code == 0 {
                        //log::debug!("lsof exited with code 0");
                        break Ok(());
                    } else {
                        // unwrap: the corresponding receiver should not be dropped yet
                        // child_exit_tx.send(code).unwrap();
                        eprintln!("lsof exited with non-zero status: {}", code);
                        exit(code);
                    }
                }
                WaitStatus::Signaled(_, signal, _) => {
                    // unwrap: the corresponding receiver should not be dropped yet
                    // use the bash convention of 128 + signal number as the exit code for
                    // processes terminated by signals
                    // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_21_18.
                    eprintln!("lsof child was terminated by signal: {:?}", signal);
                    exit(128 + signal as i32);
                }
                _ => continue,
            }
        }
    });

    if is_help(&args) {
        // just run lsof as a subprocess without visualizations
        lsof_handle
            .join()
            .map_err(|e| anyhow!("lsof thread threw an error: {:?}", e))?;
    } else {
        let options = NativeOptions {
            viewport: ViewportBuilder::default()
                .with_inner_size(Vec2::new(VIEWPORT_WIDTH, VIEWPORT_HEIGHT))
                .with_title("lvis"),
            ..Default::default()
        };
        let lsof_vis = LsofVis::new(lsof_output_rx);
        // start a little gui app
        run_native("lvis", options, Box::new(|_cc| Ok(Box::new(lsof_vis))))
            .map_err(|e| anyhow!("eframe error: {:?}", e))?;
    }

    child_handle
        .join()
        .map_err(|e| anyhow!("child handle threw an error: {:?}", e))??;
    Ok(())
}
