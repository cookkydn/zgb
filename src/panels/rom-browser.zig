const std = @import("std");
const ig = @import("cimgui");
const AppState = @import("../app.zig").AppState;

pub const RomBrowser = struct {
    visible: bool = false,
    allocator: std.mem.Allocator,

    // On stocke les noms des fichiers sous forme de chaînes terminées par 0 ([:0]u8)
    // car ImGui (qui est du C) a besoin de chaînes qui finissent par \0.
    files: std.ArrayList([:0]u8),

    // L'index du fichier actuellement surligné
    selected_idx: ?usize = null,

    // Le dossier qu'on regarde par défaut
    current_path: []const u8 = "./roms",

    pub fn init(allocator: std.mem.Allocator) RomBrowser {
        return .{
            .allocator = allocator,
            .files = std.ArrayList([:0]u8).initCapacity(allocator, 5) catch @panic("Failed allocate memory"),
        };
    }

    pub fn deinit(self: *RomBrowser) void {
        self.clearFiles();
        self.files.deinit(self.allocator);
    }

    // Vide la liste et libère la mémoire de chaque chaîne
    fn clearFiles(self: *RomBrowser) void {
        for (self.files.items) |file| {
            self.allocator.free(file);
        }
        self.files.clearRetainingCapacity();
    }

    // Scanne le dossier et met à jour la liste
    pub fn refresh(self: *RomBrowser) void {
        self.clearFiles();
        self.selected_idx = null;

        // On ouvre le dossier
        var dir = std.fs.cwd().openDir(self.current_path, .{ .iterate = true }) catch |err| {
            std.debug.print("Erreur ouverture dossier {s} : {}\n", .{ self.current_path, err });
            return;
        };
        defer dir.close();

        // On itère sur les fichiers
        var iter = dir.iterate();
        while (iter.next() catch null) |entry| {
            if (entry.kind == .file) {
                const name = entry.name;

                // On filtre : seulement les .gb ou .gbc
                if (std.mem.endsWith(u8, name, ".gb") or std.mem.endsWith(u8, name, ".gbc") or std.mem.endsWith(u8, name, ".rom")) {

                    // On duplique la string pour qu'elle survive à la fin de cette fonction
                    // et on s'assure qu'elle se termine par 0 pour ImGui (dupeZ)
                    if (self.allocator.dupeZ(u8, name)) |name_z| {
                        self.files.append(self.allocator, name_z) catch {};
                    } else |_| {}
                }
            }
        }
    }

    // La fonction appelée à chaque frame
    pub fn draw(self: *RomBrowser, app: *AppState) void {
        if (!self.visible) return;

        // Configuration de la fenêtre
        ig.igSetNextWindowSize(.{ .x = 400, .y = 300 }, ig.ImGuiCond_FirstUseEver);

        if (ig.igBegin("Sélectionner une ROM", &self.visible, ig.ImGuiWindowFlags_None)) {

            // Bouton pour recharger le dossier si on ajoute un fichier pendant que ça tourne
            if (ig.igButton("Rafraîchir")) {
                self.refresh();
            }

            ig.igSameLine();
            ig.igText("Dossier : %s", self.current_path.ptr);
            ig.igSeparator();

            // La boîte de liste (-1, -1 veut dire "remplir tout l'espace restant")
            if (ig.igBeginListBox("##roms_list", .{ .x = -1.0, .y = -1.0 })) {
                for (self.files.items, 0..) |file, i| {
                    const is_selected = (self.selected_idx != null and self.selected_idx.? == i);

                    // AllowDoubleClick permet à ImGui de traquer les doubles clics sur cet item
                    if (ig.igSelectableEx(file.ptr, is_selected, ig.ImGuiSelectableFlags_AllowDoubleClick, .{ .x = 0, .y = 0 })) {
                        // Le clic simple sélectionne l'élément
                        self.selected_idx = i;

                        // Si c'était un double clic, on charge la ROM !
                        if (ig.igIsMouseDoubleClicked(ig.ImGuiMouseButton_Left)) {
                            self.loadRom(app, file);
                        }
                    }
                }
                ig.igEndListBox();
            }
        }
        ig.igEnd();
    }

    fn loadRom(self: *RomBrowser, app: *AppState, filename: [:0]const u8) void {
        const full_path = std.fmt.allocPrintSentinel(self.allocator, "{s}/{s}", .{ self.current_path, filename }, 0) catch return;
        defer self.allocator.free(full_path);
        app.emu.cpu.bus.loadCartridge(full_path) catch @panic("Failed");
        app.emu.pause = false;
        self.visible = false;
    }
};
