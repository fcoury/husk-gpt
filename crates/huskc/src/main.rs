use clap::{Parser, Subcommand};
use std::{fs, path::Path};
use diagnostics::Reporter;
use syntax::{Lexer, Parser as HuskParser};
use resolver::Resolver;

#[derive(Parser)]
#[command(name = "huskc")]
#[command(about = "The Husk compiler")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
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
        Commands::Build { input, out_dir, target } => {
            if let Err(e) = build_file(input, out_dir, target) {
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

fn build_file(input: &str, out_dir: &str, _target: &str) -> Result<(), Box<dyn std::error::Error>> {
    let content = fs::read_to_string(input)?;
    
    let mut reporter = Reporter::new();
    let file_id = reporter.add_file(input.to_string(), content.clone());
    
    // Lex
    let mut lexer = Lexer::new(content, file_id);
    let tokens = lexer.tokenize();
    
    println!("Tokens: {:?}", tokens);
    
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
    resolver.normalize_patterns(&mut module);
    
    // TODO: Typecheck (stub)
    println!("Typechecking... (stub)");
    
    // Emit JS
    let js_code = emit_js::emit(&module);
    let js_path = Path::new(out_dir).join("main.js");
    fs::write(&js_path, js_code)?;
    
    // Emit .d.ts
    let dts_code = emit_dts::emit(&module);
    let dts_path = Path::new(out_dir).join("main.d.ts");
    fs::write(&dts_path, dts_code)?;
    
    println!("Successfully compiled {} to {}", input, out_dir);
    println!("Generated: {:?}, {:?}", js_path, dts_path);
    
    Ok(())
}

fn check_files(_files: &[String]) -> Result<(), Box<dyn std::error::Error>> {
    println!("Checking files... (not implemented yet)");
    Ok(())
}