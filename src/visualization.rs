//! Generate a GUI to display the output of the `lsof` command as a graph.
//! The graph has nodes for processes (left) and files (right) color coded.
//! The color changes by state (toggled, connected, selected, etc).

use crate::lsof::{FileConnectionMap, LsofOutputLine, stream_from_receiver};
use eframe::{App, Frame};
use egui::{
    Color32, FontId, Galley, Pos2, Rect, Sense, Stroke, StrokeKind, Ui, Vec2, mutex::RwLock,
};
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Display},
    str::FromStr,
    sync::{Arc, mpsc::Receiver},
    thread,
};
// positions top left corner of nodes
// Process nodes start and go left in a rectangle, files nodes start and go right in layout
pub const PROCESS_START_X: f32 = VIEWPORT_WIDTH / 2.0 - 3.0 * NODE_WIDTH / 2.0;
pub const FILE_START_X: f32 = VIEWPORT_WIDTH / 2.0 + NODE_WIDTH / 2.0;
pub const START_Y: f32 = VIEWPORT_HEIGHT / 5.0;
pub const NODE_WIDTH: f32 = 200.0;
pub const NODE_HEIGHT: f32 = 30.0;
pub const VIEWPORT_WIDTH: f32 = 800.0;
pub const VIEWPORT_HEIGHT: f32 = 600.0;
pub const LINE_HEIGHT: f32 = 8.0;
/// Max characters in a node label (only the command name, not including the PID)
pub const MAX_LABEL_LENGTH: usize = 15;
pub const DETAILS_PANEL_WIDTH: f32 = 300.0;
pub const DETAILS_PANEL_PADDING: f32 = 10.0;
pub const HOVER_TITLE_FONT_SIZE: f32 = 13.0;
pub const HOVER_DETAIL_FONT_SIZE: f32 = 10.0;
pub const SELECTED_TITLE_FONT_SIZE: f32 = 13.0;
pub const SELECTED_DETAIL_FONT_SIZE: f32 = 10.0;

pub struct LsofVis {
    // list of nodes have static positions (append only)
    nodes: Arc<RwLock<Vec<Node>>>,
    edges: Arc<RwLock<HashSet<Edge>>>,
    zoom: f32,
    pan: Vec2,
    curr_selected_node: Option<usize>,
    toggled_nodes: HashSet<usize>,
    connected_nodes: HashSet<usize>,
    hovering_node: Option<usize>,
    update_iteration: usize,
}

enum ReqFields {
    Command,
    Pid,
    User,
    File,
}

impl Display for ReqFields {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReqFields::Command => write!(f, "COMMAND"),
            ReqFields::Pid => write!(f, "PID"),
            ReqFields::User => write!(f, "USER"),
            ReqFields::File => write!(f, "FILE"),
        }
    }
}

impl FromStr for ReqFields {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "COMMAND" => Ok(ReqFields::Command),
            "PID" => Ok(ReqFields::Pid),
            "USER" => Ok(ReqFields::User),
            "FILE" => Ok(ReqFields::File),
            _ => Err(format!("Invalid field: {}", s)),
        }
    }
}

/// Represents either a process or a file.
/// Nodes can be either be currently selected (only one of these),
/// toggled (any previously selected node), connected (to any currently toggled node)
/// or none (none of the prior). Clicking a node moves it between selected and none,
/// and a node remains toggled until it is clicked again.
/// The color coding of the node depends on its state. Toggled and connected have the same color,
/// but the others all differ.
#[derive(Debug)]
pub struct Node {
    id: usize,
    label: String,
    full_label: String,
    node_type: NodeType,
    position: Pos2,
    size: Vec2,
    // details are displayed on click
    details: Vec<String>,
}

/// Represents a connection between nodes. Typically between a process and a file, but
/// files can be connected (e.g. PIPE or TCP where file descriptors or ports are connected)
#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Edge {
    from: usize,
    to: usize,
}

// lsof lists the open files for a process (files can be files, directories, sockets, etc)
#[derive(Debug)]
enum NodeType {
    Process,
    File,
}

impl LsofVis {
    pub fn new(lsof_output_rx: Receiver<LsofOutputLine>) -> Self {
        // starts processing nodes
        let nodes = Arc::new(RwLock::new(Vec::new()));
        let edges = Arc::new(RwLock::new(HashSet::new()));
        Self::spawn_generate_graph(lsof_output_rx, Arc::clone(&nodes), Arc::clone(&edges));

        Self {
            nodes,
            edges,
            zoom: 1.0,
            pan: Vec2::new(0.0, 0.0),
            curr_selected_node: None,
            toggled_nodes: HashSet::new(),
            connected_nodes: HashSet::new(),
            hovering_node: None,
            update_iteration: 0,
        }
    }

    /// Spawns a thread to run `lsof` and generate a graph. This populates nodes and edges.
    /// These are drawn in the [`LsofVis::update_ui`] method to the GUI.
    pub fn spawn_generate_graph(
        rx: Receiver<LsofOutputLine>,
        nodes: Arc<RwLock<Vec<Node>>>,
        edges: Arc<RwLock<HashSet<Edge>>>,
    ) {
        thread::spawn(move || {
            let mut process_nodes = HashMap::new();
            let mut file_nodes = HashMap::new();
            let mut file_conn_map = FileConnectionMap::default();
            let mut node_id: usize = 0;

            let rx = stream_from_receiver(rx);
            // Parse the table data
            while let Ok(entry) = rx.recv() {
                log::trace!("Entry for graph: {:?}", entry);
                let process_details = match entry.process_details() {
                    Ok(details) => details,
                    Err(e) => {
                        eprintln!("Error getting process details: {}", e);
                        continue;
                    }
                };
                let file_details = match entry.file_details() {
                    Ok(details) => details,
                    Err(e) => {
                        eprintln!("Error getting file details: {}", e);
                        continue;
                    }
                };
                // unwrap: after checkign details, these fields will be present
                let command = entry.command.unwrap();
                let pid = entry.pid.unwrap();
                // create nodes, identified by their command and pid
                let process_key = format!("{}-{}", command, pid);
                if !process_nodes.contains_key(&process_key) {
                    let process_label = format!("{} ({})", truncate(&command), pid);
                    let full_label = format!("{} (PID: {})", command, pid);

                    process_nodes.insert(process_key.clone(), node_id);

                    nodes.write().push(Node {
                        id: node_id,
                        label: process_label,
                        full_label,
                        node_type: NodeType::Process,
                        // Initial position, will be arranged later
                        position: Pos2::new(PROCESS_START_X, START_Y),
                        size: Vec2::new(NODE_WIDTH, NODE_HEIGHT),
                        details: process_details,
                    });

                    node_id += 1;
                }

                // create file nodes
                let file_node_key = {
                    let file_name = entry.file_name.clone().unwrap();
                    if file_name.is_empty() && entry.device.is_some() {
                        entry.device.clone().unwrap()
                    } else {
                        file_name
                    }
                };
                let process_node_id = *process_nodes.get(&process_key).unwrap();
                if !file_nodes.contains_key(&file_node_key) {
                    let display_name = truncate(&file_node_key);
                    file_nodes.insert(file_node_key.clone(), node_id);

                    // lock ordering: always acquire edges lock before nodes lock (alphabetical)
                    // Create edge
                    {
                        {
                            edges.write().insert(Edge {
                                from: process_node_id,
                                to: node_id,
                            });
                        }
                    }

                    {
                        nodes.write().push(Node {
                            id: node_id,
                            label: display_name,
                            full_label: file_node_key.clone(),
                            node_type: NodeType::File,
                            // Initial position, will be arranged later
                            position: Pos2::new(FILE_START_X, START_Y),
                            size: Vec2::new(NODE_WIDTH, NODE_HEIGHT),
                            details: file_details.clone(),
                        });
                    }

                    node_id += 1;
                } else {
                    let file_node_id: usize = *file_nodes.get(&file_node_key).unwrap();

                    // Create edge if it doesn't already exist
                    let edge = Edge {
                        from: process_node_id,
                        to: file_node_id,
                    };

                    if !edges.read().contains(&edge) {
                        edges.write().insert(edge);
                    }
                }

                // this should not error because this file node was just created
                let file_node_id = match file_nodes.get(&file_node_key) {
                    Some(id) => *id,
                    None => {
                        eprintln!("Unexpected file node for key {} missing", file_node_key);
                        continue;
                    }
                };

                // in a loose sense, this file  for the current file
                // for some file types, this doesn't make sense because there is no concept of "source and dest"
                // but for TCP connections there will be a "localhost:ABCD" file, for example, and this file is the source
                // for any connections that point to it
                file_conn_map
                    .source_to_file_nodes
                    .entry(file_node_key.clone())
                    .or_insert_with(Vec::new)
                    .push(file_node_id);

                // there is a special case for PIPEs where the device number indicates the source of the pipe. Add this to the file connection map.
                // we could limit this to just PIPEs, but do this for all types for generality
                if let Some(device) = entry.device {
                    file_conn_map
                        .source_to_file_nodes
                        .entry(device)
                        .or_insert_with(Vec::new)
                        .push(file_node_id);
                }

                // This file node could either be a raw file node (e.g. "localhost:8080"), or a connection (e.g. "localhost:8080->localhost:8081")
                // assume that it is a raw node. Then we need to add any connections that point to it and from it

                // forward connections, where this file node is a source
                if let Some(source) = file_conn_map.source_to_file_nodes.get(&file_node_key) {
                    for source_file_node_id in source {
                        edges.write().insert(Edge {
                            from: *source_file_node_id,
                            to: file_node_id,
                        });
                    }
                }

                // reverse connections, where this file node is a dest
                if let Some(dest) = file_conn_map.dest_to_file_nodes.get(&file_node_key) {
                    for dest_file_node_id in dest {
                        edges.write().insert(Edge {
                            from: file_node_id,
                            to: *dest_file_node_id,
                        });
                    }
                }

                // if this is a connection, add the connection to the file connection map
                // also add any raw file nodes that are sources or dests of this connection
                // use "localhost:8080->localhost:8081" as the running example
                if let Some(connection) = entry.connections {
                    // this should not error since this file node was just created
                    let file_node_id = match file_nodes.get(&file_node_key) {
                        Some(id) => *id,
                        None => {
                            eprintln!("Unexpected process node for key {} missing", process_key);
                            continue;
                        }
                    };
                    if let Some(source) = connection.source {
                        // add the source to the file connection map
                        // this link has "localhost:8080" as the source
                        file_conn_map
                            .source_to_file_nodes
                            .entry(source.clone())
                            .or_insert_with(Vec::new)
                            .push(file_node_id);

                        // connect to the source file node if the source file node exists
                        let source_file_node = file_nodes.get(&source);
                        if let Some(source_file_node_id) = source_file_node {
                            edges.write().insert(Edge {
                                from: *source_file_node_id,
                                to: file_node_id,
                            });
                        }
                    }
                    // `connection.dest` is connected to from this file node
                    // this link has "localhost:8081" as the dest
                    file_conn_map
                        .dest_to_file_nodes
                        .entry(connection.dest.clone())
                        .or_insert_with(Vec::new)
                        .push(file_node_id);

                    // connect to the dest file node if the dest file node exists
                    let dest_file_node = file_nodes.get(&connection.dest);
                    if let Some(dest_file_node_id) = dest_file_node {
                        edges.write().insert(Edge {
                            from: file_node_id,
                            to: *dest_file_node_id,
                        });
                    }
                }

                // Arrange nodes in a more visually pleasing layout
                Self::arrange_nodes(&mut nodes.write());
            }
        });
    }

    fn arrange_nodes(nodes: &mut [Node]) {
        // simple algorithm, separate process and resource nodes in a rectangle
        let mut process_count = 0;
        let mut resource_count = 0;

        // Layout roughly in a grid where there are 2x more rows than width, given node aspect ratios
        // Cols = x, Rows = 2x
        let x = (nodes.len() as f32 / 2.0).sqrt().ceil();
        let num_rows = 2.0 * x;

        for node in nodes.iter_mut() {
            match node.node_type {
                NodeType::Process => {
                    node.position = Pos2::new(
                        PROCESS_START_X
                            - ((process_count as f32 / num_rows).floor() * (NODE_WIDTH + 10.0)),
                        START_Y + (process_count as f32 % num_rows * (NODE_HEIGHT + 10.0)),
                    );
                    process_count += 1;
                }
                NodeType::File => {
                    node.position = Pos2::new(
                        FILE_START_X
                            + ((resource_count as f32 / num_rows).floor() * (NODE_WIDTH + 10.0)),
                        START_Y + (resource_count as f32 % num_rows * (NODE_HEIGHT + 10.0)),
                    );
                    resource_count += 1;
                }
            }
        }
    }

    fn screen_to_world(&self, pos: Pos2) -> Pos2 {
        Pos2::new(
            (pos.x - self.pan.x) / self.zoom,
            (pos.y - self.pan.y) / self.zoom,
        )
    }

    // Used to plot the nodes on the screen
    fn world_to_screen(&self, pos: Pos2) -> Pos2 {
        Pos2::new(
            pos.x * self.zoom + self.pan.x,
            pos.y * self.zoom + self.pan.y,
        )
    }

    fn update_ui(&mut self, ui: &mut Ui) {
        //log::debug!("Updating UI, iteration: {}", self.update_iteration);
        self.update_iteration += 1;
        self.handle_pan_and_zoom(ui);
        self.draw_edges(ui);
        self.check_hover(ui);
        self.draw_nodes(ui);
        self.draw_hover_details_panel(ui);
        self.draw_details_panel(ui);
        self.draw_help_text(ui);
        // create the widget
        let rect = ui.available_rect_before_wrap();
        ui.allocate_rect(rect, Sense::click_and_drag());
    }

    fn handle_pan_and_zoom(&mut self, ui: &mut Ui) {
        if ui.input(|i| i.raw_scroll_delta.y != 0.0) {
            let zoom_delta = ui.input(|i| i.raw_scroll_delta.y) * 0.001;
            let new_zoom = (self.zoom + zoom_delta).clamp(0.1, 3.0);

            // get mouse position
            let mouse_pos = ui.input(|i| {
                i.pointer.hover_pos().unwrap_or(Pos2::new(
                    ui.available_rect_before_wrap().width() / 2.0,
                    ui.available_rect_before_wrap().height() / 2.0,
                ))
            });

            // Adjust pan to zoom around mouse cursor
            // if user cursor starts at (100, 100) and we zoom in, the mouse cursor will be at (50, 50) in the world
            // but should not shift in the screen, so we pan the cursor to create this illusion
            let mouse_world_before = self.screen_to_world(mouse_pos);
            self.zoom = new_zoom;
            let mouse_world_after = self.screen_to_world(mouse_pos);

            self.pan.x += (mouse_world_after.x - mouse_world_before.x) * self.zoom;
            self.pan.y += (mouse_world_after.y - mouse_world_before.y) * self.zoom;
        }

        // handle panning with drag
        if ui.input(|i| i.pointer.middle_down() || (i.pointer.primary_down() && i.modifiers.ctrl)) {
            self.pan += ui.input(|i| i.pointer.delta());
        }
    }

    fn draw_edges(&mut self, ui: &mut Ui) {
        let painter = ui.painter();
        self.connected_nodes.clear();

        // lock ordering: always acquire edges lock before nodes lock (alphabetical)
        let edges = self.edges.read();
        let nodes = self.nodes.read();
        for edge in edges.iter() {
            let from_node = &nodes[edge.from];
            let to_node = &nodes[edge.to];

            // draw edges if either `to` or `from` node is expanded
            if !self.toggled_nodes.contains(&from_node.id)
                && !self.toggled_nodes.contains(&to_node.id)
            {
                continue;
            }
            // toggled nodes create edges which create connections
            self.connected_nodes.insert(from_node.id);
            self.connected_nodes.insert(to_node.id);

            let from_pos = self.world_to_screen(
                from_node.position + Vec2::new(from_node.size.x / 2.0, from_node.size.y / 2.0),
            );
            let to_pos = self.world_to_screen(
                to_node.position + Vec2::new(to_node.size.x / 2.0, to_node.size.y / 2.0),
            );

            // draw edge, edges have no text
            painter.line_segment(
                [from_pos, to_pos],
                Stroke::new(1.0, Color32::from_gray(150)),
            );
        }
    }

    fn check_hover(&mut self, ui: &mut Ui) {
        // check for node hover, determines if clicks are in a node
        self.hovering_node = None;
        for (i, node) in self.nodes.read().iter().enumerate() {
            let node_rect =
                Rect::from_min_size(self.world_to_screen(node.position), node.size * self.zoom);

            if ui.input(|i| i.pointer.hover_pos().is_some_and(|p| node_rect.contains(p))) {
                self.hovering_node = Some(i);
            }
        }
    }

    fn draw_nodes(&mut self, ui: &mut Ui) {
        let rect = ui.available_rect_before_wrap();
        let painter = ui.painter();
        for (node_idx, node) in self.nodes.read().iter().enumerate() {
            let node_pos = self.world_to_screen(node.position);
            let node_size = node.size * self.zoom;
            let node_rect = Rect::from_min_size(node_pos, node_size);

            // skip non-visible nodes
            if !rect.intersects(node_rect) {
                continue;
            }

            // determine node color
            let fill_color = match node.node_type {
                NodeType::Process => {
                    if self.curr_selected_node == Some(node_idx) {
                        // yellow for current node
                        Color32::from_rgb(209, 151, 65)
                    } else if self.toggled_nodes.contains(&node_idx) {
                        // medium blue for toggled nodes
                        // Color32::from_rgb(16, 26, 68)
                        Color32::from_rgb(12, 60, 128)
                    } else if self.connected_nodes.contains(&node_idx) {
                        // light blue for connected nodes
                        Color32::from_rgb(59, 68, 210)
                    } else {
                        // gray for other process nodes
                        Color32::from_rgb(59, 68, 75)
                    }
                }
                NodeType::File => {
                    if self.curr_selected_node == Some(node_idx) {
                        // yellow for current node
                        Color32::from_rgb(209, 151, 65)
                    } else if self.toggled_nodes.contains(&node_idx) {
                        // medium green for toggled nodes
                        // Color32::from_rgb(4, 47, 23)
                        Color32::from_rgb(12, 107, 53)
                    } else if self.connected_nodes.contains(&node_idx) {
                        // light green for connected nodes
                        Color32::from_rgb(39, 174, 96)
                    } else {
                        // gray for other file nodes
                        Color32::from_rgb(59, 68, 75)
                    }
                }
            };

            let shadow_offset = if self.curr_selected_node == Some(node_idx) {
                4.0
            } else {
                2.0
            };

            // draw node shadow
            painter.rect(
                Rect::from_min_size(
                    node_pos + Vec2::new(shadow_offset, shadow_offset),
                    node_size,
                ),
                5.0,
                Color32::from_black_alpha(100),
                Stroke::NONE,
                StrokeKind::Inside,
            );

            // draw node
            painter.rect(
                node_rect,
                5.0,
                fill_color,
                Stroke::new(1.0, Color32::from_gray(50)),
                StrokeKind::Inside,
            );

            // Draw node label
            painter.text(
                node_rect.center(),
                egui::Align2::CENTER_CENTER,
                &node.label,
                FontId::proportional(14.0 * self.zoom.min(1.0)),
                Color32::WHITE,
            );

            // Check for node click
            if ui.input(|i| i.pointer.primary_released()) && self.hovering_node == Some(node_idx) {
                // toggle expansion of node
                if self.toggled_nodes.contains(&node_idx) {
                    self.toggled_nodes.remove(&node_idx);
                    self.curr_selected_node = None;
                } else {
                    self.toggled_nodes.insert(node_idx);
                    self.curr_selected_node = Some(node_idx)
                }
            }
        }
    }

    fn draw_hover_details_panel(&self, ui: &mut Ui) {
        // Only draw if we have a hovered node and it's not the selected node
        if let Some(node_idx) = self.hovering_node {
            if self.curr_selected_node == Some(node_idx) {
                return; // Don't draw hover panel for the selected node
            }

            let node = &self.nodes.read()[node_idx];
            let node_pos = self.world_to_screen(node.position);
            let node_size = node.size * self.zoom;

            // Position the panel just below the node
            let panel_rect = Rect::from_min_size(
                Pos2::new(node_pos.x, node_pos.y + node_size.y + 5.0),
                Vec2::new(DETAILS_PANEL_WIDTH, 0.0), // Height will be determined by content
            );
            Self::draw_node_detail_in_panel(
                ui,
                panel_rect,
                node,
                HOVER_TITLE_FONT_SIZE,
                HOVER_DETAIL_FONT_SIZE,
            );
        }
    }

    fn draw_details_panel(&self, ui: &mut Ui) {
        // Only draw details if we have a selected node
        if let Some(node_idx) = self.curr_selected_node {
            let node = &self.nodes.read()[node_idx];
            let rect = ui.available_rect_before_wrap();

            // put the panel in the top right corner
            let panel_rect = Rect::from_min_size(
                Pos2::new(
                    rect.max.x - DETAILS_PANEL_WIDTH - DETAILS_PANEL_PADDING,
                    rect.min.y + DETAILS_PANEL_PADDING,
                ),
                Vec2::new(DETAILS_PANEL_WIDTH, 0.0), // Height will be determined by content
            );

            Self::draw_node_detail_in_panel(
                ui,
                panel_rect,
                node,
                SELECTED_TITLE_FONT_SIZE,
                SELECTED_DETAIL_FONT_SIZE,
            );
        }
    }

    // Draws a node detail inside a positioned panel
    fn draw_node_detail_in_panel(
        ui: &mut Ui,
        panel_rect: Rect,
        node: &Node,
        title_font_size: f32,
        detail_font_size: f32,
    ) {
        let painter = ui.painter();
        let max_width = panel_rect.width() - (DETAILS_PANEL_PADDING * 2.0);

        let mut galleys: Vec<(Pos2, Arc<Galley>)> = Vec::new();
        let title_galley = Self::create_wrapped_text(
            ui.ctx(),
            &node.full_label,
            max_width,
            FontId::proportional(title_font_size),
            Color32::WHITE,
            egui::Align::Min,
            // larger line height for title
            LINE_HEIGHT * 1.25,
        );
        let title_height = title_galley.rect.height();
        galleys.push((
            Pos2::new(
                panel_rect.min.x + DETAILS_PANEL_PADDING,
                panel_rect.min.y + DETAILS_PANEL_PADDING,
            ),
            title_galley,
        ));

        let mut y_pos = panel_rect.min.y + title_height + DETAILS_PANEL_PADDING * 2.0;

        for detail in node.details.iter() {
            let detail_galley = Self::create_wrapped_text(
                ui.ctx(),
                detail,
                max_width,
                FontId::proportional(detail_font_size),
                Color32::from_gray(230),
                egui::Align::Min,
                LINE_HEIGHT,
            );
            let detail_height = detail_galley.rect.height();

            galleys.push((
                Pos2::new(panel_rect.min.x + DETAILS_PANEL_PADDING, y_pos),
                detail_galley,
            ));

            y_pos += detail_height + DETAILS_PANEL_PADDING;
        }

        // draw panel background
        let panel_height = y_pos - panel_rect.min.y;
        painter.rect(
            Rect::from_min_size(panel_rect.min, Vec2::new(panel_rect.width(), panel_height)),
            5.0,
            Color32::from_rgba_premultiplied(30, 30, 30, 230),
            Stroke::new(1.0, Color32::from_gray(100)),
            StrokeKind::Inside,
        );

        for (pos, galley) in galleys {
            painter.galley(pos, galley.clone(), Color32::WHITE);
        }
    }

    fn create_wrapped_text(
        ctx: &egui::Context,
        text: &str,
        max_width: f32,
        font_id: egui::FontId,
        color: egui::Color32,
        align: egui::Align,
        line_height: f32,
    ) -> Arc<Galley> {
        let mut job = egui::text::LayoutJob::default();
        job.append(
            text,
            0.0,
            egui::text::TextFormat {
                font_id,
                color,
                line_height: Some(line_height),
                ..Default::default()
            },
        );
        job.wrap.max_width = max_width;
        job.halign = align;

        ctx.fonts(|f| f.layout_job(job))
    }

    fn draw_help_text(&mut self, ui: &mut Ui) {
        let painter = ui.painter();
        let rect = ui.available_rect_before_wrap();

        let help_text = "\
Wheel = Zoom
Middle Btn or Ctrl+Drag = Pan
Click Node = Show Details
Left (blue) = Commands, Right (green) = Files";

        painter.text(
            Pos2::new(rect.left() + 10.0, rect.top() + 10.0),
            egui::Align2::LEFT_TOP,
            help_text,
            FontId::proportional(14.0),
            Color32::from_gray(200),
        );
    }
}

impl App for LsofVis {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            self.update_ui(ui);
        });
    }
}

fn truncate(s: &str) -> String {
    if s.len() > MAX_LABEL_LENGTH {
        s[0..MAX_LABEL_LENGTH].to_string() + "..."
    } else {
        s.to_string()
    }
}
