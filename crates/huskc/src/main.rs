use clap::{Parser, Subcommand};
use diagnostics::Reporter;
use resolver::Resolver;
use std::{fs, path::Path};
use syntax::{Lexer, Parser as HuskParser};

#[derive(Parser)]
#[command(name = "huskc")]
#[command(about = "The Husk compiler")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
    /// Enable verbose output
    #[arg(long, short, global = true)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    Build {
        input: String,
        #[arg(long, default_value = "dist")]
        out_dir: String,
        #[arg(long, default_value = "es2020")]
        target: String,
    },
    Check {
        #[arg(value_name = "FILES")]
        files: Vec<String>,
    },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Build {
            input,
            out_dir,
            target,
        } => {
            if let Err(e) = build_file(input, out_dir, target, cli.verbose) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
        Commands::Check { files } => {
            if let Err(e) = check_files(files) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
    }
}

fn build_file(input: &str, out_dir: &str, _target: &str, verbose: bool) -> Result<(), Box<dyn std::error::Error>> {
    let content = fs::read_to_string(input)?;

    let mut reporter = Reporter::new();
    let file_id = reporter.add_file(input.to_string(), content.clone());

    // Lex
    let mut lexer = Lexer::new(content, file_id);
    let tokens = lexer.tokenize();

    if verbose {
        println!("Tokens: {:?}", tokens);
    }

    // Parse
    let mut parser = HuskParser::new(tokens);
    let (mut module, diagnostics) = parser.parse();

    // Report diagnostics
    for diagnostic in diagnostics {
        reporter.report(diagnostic);
    }

    if reporter.has_errors() {
        reporter.print_all();
        return Err("Compilation failed with errors".into());
    }

    // Create output directory
    fs::create_dir_all(out_dir)?;

    // Add prelude items to make Option/Result available
    let prelude = corelib::get_prelude_module();
    for item in prelude.items {
        module.items.insert(0, item);
    }

    // Name resolution
    let mut resolver = Resolver::new();
    let resolution_diagnostics = resolver.resolve(&module);

    // Report resolution diagnostics
    for diagnostic in resolution_diagnostics {
        reporter.report(diagnostic);
    }

    if reporter.has_errors() {
        reporter.print_all();
        return Err("Name resolution failed with errors".into());
    }

    println!("Name resolution complete!");

    // Normalize patterns (convert unit variant identifiers to variant patterns)
    // and collect known variants from resolver for emitter safety net
    let known_variants = resolver.known_variant_names();
    resolver.normalize_patterns(&mut module);

    // TODO: Typecheck (stub)
    println!("Typechecking... (stub)");

    // Derive output file names from input, preserving directory structure
    let input_path = Path::new(input);
    let file_stem = input_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main");

    // If input has a directory structure, preserve it in output
    let relative_dir = if let Some(parent) = input_path.parent() {
        if parent != Path::new("") && parent != Path::new(".") {
            Some(parent)
        } else {
            None
        }
    } else {
        None
    };

    let output_base = if let Some(dir) = relative_dir {
        Path::new(out_dir).join(dir)
    } else {
        Path::new(out_dir).to_path_buf()
    };

    // Ensure output directory exists
    fs::create_dir_all(&output_base)?;

    // Emit JS
    let js_code = emit_js::emit_with_known_variants(&module, &known_variants);
    let js_path = output_base.join(format!("{}.js", file_stem));
    fs::write(&js_path, js_code)?;

    // Emit .d.ts
    let dts_code = emit_dts::emit(&module);
    let dts_path = output_base.join(format!("{}.d.ts", file_stem));
    fs::write(&dts_path, dts_code)?;

    println!("Successfully compiled {} to {}", input, out_dir);
    println!("Generated: {:?}, {:?}", js_path, dts_path);

    Ok(())
}

fn check_files(_files: &[String]) -> Result<(), Box<dyn std::error::Error>> {
    println!("Checking files... (not implemented yet)");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_prelude_integration_smoke_test() {
        // Test that Option/Result are available without local definition via prelude
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        
        // Create input file at temp dir root to avoid complex path preservation
        let input_file = temp_dir.path().join("test_prelude.hk");
        let out_dir = temp_dir.path().join("dist");
        
        let source_code = r#"
// Test prelude types work without local definition
fn test_option(x: Option) {
    match x {
        Some(value) => value,
        None => "empty"
    }
}

fn test_result(x: Result) {
    match x {
        Ok(value) => value,
        Err(msg) => msg
    }
}

// Test qualified patterns with prelude types
fn test_qualified(x: Option) {
    match x {
        Option::Some(value) => value,
        Option::None => "none"
    }
}
        "#;

        fs::write(&input_file, source_code).expect("Failed to write test file");

        // Should compile successfully without errors
        let result = build_file(
            input_file.to_str().unwrap(),
            out_dir.to_str().unwrap(), 
            "es2020", 
            false
        );

        assert!(result.is_ok(), "Prelude integration should allow Option/Result without local definitions");

        // Verify output files were generated (replicate the exact logic from build_file)
        let input_path = std::path::Path::new(input_file.to_str().unwrap());
        let file_stem = input_path.file_stem().and_then(|s| s.to_str()).unwrap_or("main");
        
        // Replicate the directory preservation logic from build_file
        let relative_dir = if let Some(parent) = input_path.parent() {
            if parent != std::path::Path::new("") && parent != std::path::Path::new(".") {
                Some(parent)
            } else {
                None
            }
        } else {
            None
        };

        let output_base = if let Some(dir) = relative_dir {
            out_dir.join(dir)
        } else {
            out_dir.clone()
        };
        
        let js_file = output_base.join(format!("{}.js", file_stem));
        let dts_file = output_base.join(format!("{}.d.ts", file_stem));
        
        assert!(js_file.exists(), "JS file should be generated at {:?}", js_file);
        assert!(dts_file.exists(), "TypeScript declaration file should be generated at {:?}", dts_file);

        // Verify the generated JS contains the prelude types
        let js_content = fs::read_to_string(&js_file).expect("Failed to read generated JS");
        assert!(js_content.contains("Option"), "Generated JS should contain Option from prelude");
        assert!(js_content.contains("Result"), "Generated JS should contain Result from prelude");
        assert!(js_content.contains("Some"), "Generated JS should contain Some variant");
        assert!(js_content.contains("None"), "Generated JS should contain None variant");
    }
}
