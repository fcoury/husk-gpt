/// Shared utility for rewriting import paths across JavaScript and TypeScript declaration outputs
pub fn rewrite_import_path_for_js(path: &str) -> String {
    // If it's a relative path (./ or ../), add .js extension
    if path.starts_with("./") || path.starts_with("../") {
        // Check if it already has an extension
        if path.ends_with(".hk") {
            // Replace .hk with .js for Husk source files
            path.replace(".hk", ".js")
        } else if path.ends_with('/') {
            // Directory import, leave as-is
            path.to_string()
        } else if path
            .rfind('.')
            .map_or(true, |dot_pos| dot_pos < path.rfind('/').unwrap_or(0))
        {
            // No extension after the last slash, add .js
            format!("{}.js", path)
        } else {
            // Has extension, keep as-is
            path.to_string()
        }
    } else if path.starts_with('/') {
        // Absolute paths - add .js extension if needed
        if path.ends_with(".hk") {
            path.replace(".hk", ".js")
        } else if path.ends_with('/') {
            path.to_string()
        } else if path
            .rfind('.')
            .map_or(true, |dot_pos| dot_pos < path.rfind('/').unwrap_or(0))
        {
            format!("{}.js", path)
        } else {
            path.to_string()
        }
    } else {
        // Package imports (no ./ or ../) - leave as-is for node_modules
        path.to_string()
    }
}

pub fn rewrite_import_path_for_dts(path: &str) -> String {
    // For TypeScript .d.ts files, we don't add extensions but remove .hk
    if path.starts_with("./") || path.starts_with("../") {
        if path.ends_with(".hk") {
            // Remove .hk extension for TypeScript declarations
            path.replace(".hk", "")
        } else {
            // Keep as-is for other paths
            path.to_string()
        }
    } else if path.starts_with('/') {
        // Absolute paths - remove .hk if present
        if path.ends_with(".hk") {
            path.replace(".hk", "")
        } else {
            path.to_string()
        }
    } else {
        // Package imports - leave as-is
        path.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_js_path_rewriting() {
        assert_eq!(rewrite_import_path_for_js("./module.hk"), "./module.js");
        assert_eq!(rewrite_import_path_for_js("../utils.hk"), "../utils.js");
        assert_eq!(rewrite_import_path_for_js("./components/button"), "./components/button.js");
        assert_eq!(rewrite_import_path_for_js("react"), "react"); // Package imports unchanged
    }

    #[test]
    fn test_dts_path_rewriting() {
        assert_eq!(rewrite_import_path_for_dts("./module.hk"), "./module");
        assert_eq!(rewrite_import_path_for_dts("../utils.hk"), "../utils");
        assert_eq!(rewrite_import_path_for_dts("./components/button"), "./components/button");
        assert_eq!(rewrite_import_path_for_dts("react"), "react"); // Package imports unchanged
    }
}