const ig = @import("cimgui");

/// Defines the behavior and appearance of ImGui windows (via `igBegin`).
/// This is the core enumeration used to configure the main panels of the UI.
///
/// Usage: Pass the combined value to the `flags` parameter of `igBegin`.
pub const WindowFlag = enum(c_int) {
    /// Default state, no flags enabled.
    None = ig.ImGuiWindowFlags_None,

    /// Disables the title bar at the top of the window.
    NoTitleBar = ig.ImGuiWindowFlags_NoTitleBar,
    /// Prevents the user from collapsing the window by double-clicking the title bar.
    NoCollapse = ig.ImGuiWindowFlags_NoCollapse,
    /// Makes the window background completely transparent and removes borders.
    NoBackground = ig.ImGuiWindowFlags_NoBackground,
    /// Adds a menu bar to the top of the window (requires ig.igBeginMenuBar).
    MenuBar = ig.ImGuiWindowFlags_MenuBar,
    /// Displays a dot next to the window title to indicate unsaved changes.
    UnsavedDocument = ig.ImGuiWindowFlags_UnsavedDocument,

    /// Prevents the user from manually resizing the window via the lower-right grip.
    NoResize = ig.ImGuiWindowFlags_NoResize,
    /// Prevents the user from manually moving the window by dragging the title bar.
    NoMove = ig.ImGuiWindowFlags_NoMove,
    /// Automatically resizes the window every frame to fit its content exactly.
    AlwaysAutoResize = ig.ImGuiWindowFlags_AlwaysAutoResize,
    /// Prevent the window from docking
    NoDocking = ig.ImGuiWindowFlags_NoDocking,

    /// Hides the scrollbars, even if the content exceeds the window size.
    NoScrollbar = ig.ImGuiWindowFlags_NoScrollbar,
    /// Prevents the user from scrolling the window using the mouse wheel.
    NoScrollWithMouse = ig.ImGuiWindowFlags_NoScrollWithMouse,
    /// Allows a horizontal scrollbar to appear if content is too wide.
    HorizontalScrollbar = ig.ImGuiWindowFlags_HorizontalScrollbar,
    /// Forces the vertical scrollbar to always be visible, regardless of content size.
    AlwaysVerticalScrollbar = ig.ImGuiWindowFlags_AlwaysVerticalScrollbar,
    /// Forces the horizontal scrollbar to always be visible, regardless of content size.
    AlwaysHorizontalScrollbar = ig.ImGuiWindowFlags_AlwaysHorizontalScrollbar,

    /// Prevents the window from automatically taking focus when it transitions from hidden to visible.
    NoFocusOnAppearing = ig.ImGuiWindowFlags_NoFocusOnAppearing,
    /// Prevents the window from being brought to the front when clicked.
    NoBringToFrontOnFocus = ig.ImGuiWindowFlags_NoBringToFrontOnFocus,
    /// Ignores all mouse interactions (clicks and hovers pass through to the window behind).
    NoMouseInputs = ig.ImGuiWindowFlags_NoMouseInputs,
    /// Ignores gamepad and keyboard navigation inputs within this window.
    NoNavInputs = ig.ImGuiWindowFlags_NoNavInputs,
    /// Prevents gamepad and keyboard navigation from focusing this window.
    NoNavFocus = ig.ImGuiWindowFlags_NoNavFocus,

    /// Prevents the window's position, size, and state from being saved to the imgui.ini file.
    NoSavedSettings = ig.ImGuiWindowFlags_NoSavedSettings,

    /// Combo: Disables all gamepad/keyboard navigation (NoNavInputs | NoNavFocus).
    NoNav = ig.ImGuiWindowFlags_NoNav,
    /// Combo: Removes all visual window controls (NoTitleBar | NoResize | NoScrollbar | NoCollapse).
    NoDecoration = ig.ImGuiWindowFlags_NoDecoration,
    /// Combo: Disables all user inputs (NoMouseInputs | NoNavInputs | NoNavFocus).
    NoInputs = ig.ImGuiWindowFlags_NoInputs,

    /// Prevents scrollbars and accidental collapsing.
    EmuScreen = ig.ImGuiWindowFlags_NoScrollbar | ig.ImGuiWindowFlags_NoCollapse,
    /// Automatically hugs the content and cannot be collapsed.
    SettingsPanel = ig.ImGuiWindowFlags_AlwaysAutoResize | ig.ImGuiWindowFlags_NoCollapse,
    /// Transparent, ignores input, fits content, and doesn't steal focus.
    Overlay = ig.ImGuiWindowFlags_NoDecoration | ig.ImGuiWindowFlags_NoBackground |
        ig.ImGuiWindowFlags_NoInputs | ig.ImGuiWindowFlags_NoSavedSettings |
        ig.ImGuiWindowFlags_AlwaysAutoResize | ig.ImGuiWindowFlags_NoFocusOnAppearing,

    _,
};

/// Defines the behavior and appearance of ImGui tables (via `igBeginTable`).
/// The Table API is the modern and most performant way to display complex data grids.
///
/// Usage: Pass the combined value to the `flags` parameter of `igBeginTable`.
pub const TableFlag = enum(c_int) {
    /// Default state, no flags enabled.
    None = ig.ImGuiTableFlags_None,

    /// Draw all borders (outer and inner, horizontal and vertical).
    Borders = ig.ImGuiTableFlags_Borders,
    /// Draw vertical borders between columns.
    BordersInnerV = ig.ImGuiTableFlags_BordersInnerV,
    /// Append a background color to alternating rows (zebra striping) for better readability.
    RowBg = ig.ImGuiTableFlags_RowBg,

    /// Allow the user to manually resize columns.
    Resizable = ig.ImGuiTableFlags_Resizable,
    /// Enable vertical scrolling.
    ScrollY = ig.ImGuiTableFlags_ScrollY,
    /// Columns default to tightly fitting their contents (ideal for hex dumps).
    SizingFixedFit = ig.ImGuiTableFlags_SizingFixedFit,
    /// Columns default to stretching to fill available width proportionally.
    SizingStretchProp = ig.ImGuiTableFlags_SizingStretchProp,

    _,
};

/// Defines the restrictions and behavior of text input fields (via `igInputText`).
/// Allows precise control over what the user can type and how the field reacts.
///
/// Usage: Pass the combined value to the `flags` parameter of `igInputText`.
pub const InputTextFlag = enum(c_int) {
    /// Default state, no flags enabled.
    None = ig.ImGuiInputTextFlags_None,

    /// Allow only digits (0-9).
    CharsDecimal = ig.ImGuiInputTextFlags_CharsDecimal,
    /// Allow only hexadecimal characters (0-9, a-f, A-F).
    CharsHexadecimal = ig.ImGuiInputTextFlags_CharsHexadecimal,
    /// Automatically convert all input characters to uppercase.
    CharsUppercase = ig.ImGuiInputTextFlags_CharsUppercase,

    /// The input function will only return true when the user presses the Enter key.
    EnterReturnsTrue = ig.ImGuiInputTextFlags_EnterReturnsTrue,
    /// Automatically select the entire text when the input field is clicked.
    AutoSelectAll = ig.ImGuiInputTextFlags_AutoSelectAll,

    /// Forces uppercase hex, selects all text on click for quick rewriting, and validates on Enter.
    AddressInput = ig.ImGuiInputTextFlags_CharsHexadecimal |
        ig.ImGuiInputTextFlags_CharsUppercase |
        ig.ImGuiInputTextFlags_AutoSelectAll |
        ig.ImGuiInputTextFlags_EnterReturnsTrue,

    _,
};

/// Defines the appearance and behavior of collapsible sections (via `igTreeNodeEx`).
/// Tree nodes help organize the UI hierarchically to prevent visual clutter.
///
/// Usage: Pass the combined value to the `flags` parameter of `igTreeNodeEx_Str`.
pub const TreeNodeFlag = enum(c_int) {
    /// Default state, no flags enabled.
    None = ig.ImGuiTreeNodeFlags_None,

    /// Draw a frame with background color around the node title (looks like a collapsing header).
    Framed = ig.ImGuiTreeNodeFlags_Framed,
    /// Set the node to be open by default when it is first created.
    DefaultOpen = ig.ImGuiTreeNodeFlags_DefaultOpen,

    /// Draw the node with a selected (highlighted) background.
    Selected = ig.ImGuiTreeNodeFlags_Selected,
    /// The node will only toggle its open state when clicking the arrow icon, not the text label.
    OpenOnArrow = ig.ImGuiTreeNodeFlags_OpenOnArrow,
    /// The node will toggle its open state when double-clicking the text label.
    OpenOnDoubleClick = ig.ImGuiTreeNodeFlags_OpenOnDoubleClick,

    /// Indicates that this node has no children (removes the expansion arrow icon).
    Leaf = ig.ImGuiTreeNodeFlags_Leaf,

    _,
};

/// Combines a tuple of flags into a single `c_int` value.
///
/// Use this function to pass multiple flags to ImGui functions without needing
/// to manually cast each enum field with `@intFromEnum()`.
///
/// Example usage:
/// ```zig
/// const flags = combine(.{ WindowFlag.AlwaysAutoResize, WindowFlag.MenuBar });
/// if (!ig.igBegin("My window", null, flags)) return;
/// ```
pub fn combine(comptime flags: anytype) c_int {
    var res: c_int = 0;
    inline for (flags) |f| {
        res |= @intFromEnum(f);
    }
    return res;
}
