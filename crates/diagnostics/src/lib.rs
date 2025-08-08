use std::collections::HashMap;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub file_id: u32,
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(file_id: u32, start: u32, end: u32) -> Self {
        Self { file_id, start, end }
    }

    pub fn len(&self) -> u32 {
        self.end - self.start
    }
}

#[derive(Debug, Clone)]
pub enum DiagKind {
    Error(ErrorCode),
    Warning(WarningCode),
    Note,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCode {
    ParseError,
    TypeError,
    UnresolvedName,
    DuplicateDefinition,
    NameResolutionError,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WarningCode {
    UnusedVariable,
    DeadCode,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub kind: DiagKind,
    pub span: Span,
    pub message: String,
    pub labels: Vec<(Span, String)>,
}

impl Diagnostic {
    pub fn error(code: ErrorCode, span: Span, message: impl Into<String>) -> Self {
        Self {
            kind: DiagKind::Error(code),
            span,
            message: message.into(),
            labels: Vec::new(),
        }
    }

    pub fn warning(code: WarningCode, span: Span, message: impl Into<String>) -> Self {
        Self {
            kind: DiagKind::Warning(code),
            span,
            message: message.into(),
            labels: Vec::new(),
        }
    }

    pub fn with_label(mut self, span: Span, message: impl Into<String>) -> Self {
        self.labels.push((span, message.into()));
        self
    }
}

pub struct Reporter {
    diagnostics: Vec<Diagnostic>,
    files: HashMap<u32, (String, String)>, // file_id -> (name, content)
    next_file_id: u32,
}

impl Reporter {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
            files: HashMap::new(),
            next_file_id: 0,
        }
    }

    pub fn add_file(&mut self, name: String, content: String) -> u32 {
        let file_id = self.next_file_id;
        self.next_file_id += 1;
        self.files.insert(file_id, (name, content));
        file_id
    }

    pub fn report(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| matches!(d.kind, DiagKind::Error(_)))
    }

    pub fn print_all(&self) {
        for diagnostic in &self.diagnostics {
            self.print_diagnostic(diagnostic);
        }
    }

    fn print_diagnostic(&self, diagnostic: &Diagnostic) {
        let (file_name, file_content) = &self.files[&diagnostic.span.file_id];
        
        let report_kind = match diagnostic.kind {
            DiagKind::Error(_) => ReportKind::Error,
            DiagKind::Warning(_) => ReportKind::Warning,
            DiagKind::Note => ReportKind::Advice,
        };

        let mut report = Report::build(report_kind, file_name, diagnostic.span.start as usize)
            .with_message(&diagnostic.message)
            .with_label(
                Label::new((file_name, diagnostic.span.start as usize..diagnostic.span.end as usize))
                    .with_message(&diagnostic.message)
                    .with_color(match diagnostic.kind {
                        DiagKind::Error(_) => Color::Red,
                        DiagKind::Warning(_) => Color::Yellow,
                        DiagKind::Note => Color::Blue,
                    })
            );

        for (span, message) in &diagnostic.labels {
            report = report.with_label(
                Label::new((file_name, span.start as usize..span.end as usize))
                    .with_message(message)
                    .with_color(Color::Cyan)
            );
        }

        report
            .finish()
            .print((file_name, Source::from(file_content)))
            .unwrap();
    }
}

impl Default for Reporter {
    fn default() -> Self {
        Self::new()
    }
}
