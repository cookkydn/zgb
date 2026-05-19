const flag_mod = @import("flags.zig");
const fmt = @import("fmt.zig");
const ig = @import("cimgui");
const std = @import("std");
const TableFlag = flag_mod.TableFlag;
const combine = flag_mod.combine;

/// Safely manages the lifecycle of an ImGui table.
///
/// Example:
/// ```zig
/// const table = ui.tables.begin("MyTable", 3, ui.flags.TableFlag.Borders);
/// defer table.end();
/// if (table.visible) {
///     // Draw rows and cells...
/// }
/// ```
pub const Table = struct {
    visible: bool,

    /// Begins a new table. Returns a struct with a `visible` field.
    /// You must check `if (table.visible)` before drawing contents,
    /// but `defer table.end()` is always safe to call.
    pub fn begin(str_id: [*c]const u8, columns_count: c_int, flags: c_int) Table {
        const visible = ig.igBeginTable(str_id, columns_count, flags, .{ .x = 0, .y = 0 }, 0.0);
        return .{ .visible = visible };
    }

    /// Setup a column with specific flags and name. Call this before `headersRow`.
    pub fn setupColumn(self: Table, label: [*c]const u8, flags: c_int) void {
        _ = self;
        ig.igTableSetupColumn(label, flags, 0.0, 0);
    }

    /// Submits the header row based on previously setup columns.
    pub fn headersRow(self: Table) void {
        _ = self;
        ig.igTableHeadersRow();
    }

    /// Moves to the next row in the table.
    pub fn nextRow(self: Table) void {
        _ = self;
        ig.igTableNextRow(0, 0.0);
    }

    /// Moves to the next column in the current row.
    pub fn nextColumn(self: Table) bool {
        _ = self;
        return ig.igTableNextColumn();
    }

    /// Ends the table. Safe to call even if `begin` returned visible = false.
    pub fn end(self: Table) void {
        if (self.visible) {
            ig.igEndTable();
        }
    }
};

/// Specialized wrapper for an Hexadecimal Memory Dump (16 bytes per row).
/// Automatically configures the 17 columns (1 for Address, 16 for Bytes).
pub const MemoryTable = struct {
    table: Table,

    /// Begins a memory viewer table.
    pub fn begin(str_id: [*c]const u8) MemoryTable {
        const flags = [_]TableFlag{ .BordersInnerV, .RowBg, .ScrollY, .SizingFixedFit };
        const table = Table.begin(str_id, 17, combine(flags));

        if (table.visible) {
            // Setup columns
            table.setupColumn("Address", ig.ImGuiTableColumnFlags_NoHide);

            // Setup 16 hex columns (00 to 0F)
            comptime var i: u8 = 0;
            inline while (i < 16) : (i += 1) {
                var buf: [3]u8 = undefined;
                const header = std.fmt.bufPrintZ(&buf, "{X:0>2}", .{i}) catch unreachable;
                table.setupColumn(header.ptr, ig.ImGuiTableColumnFlags_None);
            }

            table.headersRow();
        }

        return .{ .table = table };
    }

    /// Ends the memory table.
    pub fn end(self: MemoryTable) void {
        self.table.end();
    }

    /// Draws a single row of memory (Address + up to 16 bytes).
    /// Pass a slice of bytes. Unmapped or unread memory is handled safely.
    pub fn drawRow(self: MemoryTable, base_address: u16, data: []const u8) void {
        _ = self;
        ig.igTableNextRow();

        // 1. Address Column
        _ = ig.igTableNextColumn();
        fmt.colored(0xFFA9DCA5, "0x{X:0>4}", .{base_address});

        // 2. Data Columns
        var i: usize = 0;
        while (i < 16) : (i += 1) {
            _ = ig.igTableNextColumn();

            if (i < data.len) {
                const byte = data[i];
                if (byte == 0x00) {
                    fmt.disabled("{X:0>2}", .{byte});
                } else {
                    fmt.print("{X:0>2}", .{byte});
                }
            } else {
                // Unmapped or out-of-bounds memory
                fmt.disabled("??", .{});
            }
        }
    }
};
