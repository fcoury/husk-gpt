use crate::Spanned;

#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Spanned<Item>>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
    Struct(Struct),
    Enum(Enum),
    TypeAlias(TypeAlias),
    Interface(Interface),
    ExternMod(ExternMod),
    Import(Import),
    Export(Export),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub vis: Visibility,
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub vis: Visibility,
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub vis: Visibility,
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub vis: Visibility,
    pub name: String,
    pub variants: Vec<Variant>,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub vis: Visibility,
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Interface {
    pub vis: Visibility,
    pub name: String,
    pub methods: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct ExternMod {
    pub name: String,
    pub items: Vec<Spanned<Item>>,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub path: String,
    pub items: ImportClause,
}

#[derive(Debug, Clone)]
pub enum ImportClause {
    Named(Vec<ImportItem>),
    Namespace(String),  // import * as name
    Default(String),    // import defaultName
    Mixed { default: String, named: Vec<ImportItem> }, // import defaultName, { named }
}

#[derive(Debug, Clone)]
pub struct ImportItem {
    pub name: String,
    pub alias: Option<String>,
}

#[derive(Debug, Clone)]
pub enum Export {
    Item(Box<Spanned<Item>>),     // export fn foo() {}
    Named(Vec<ExportItem>),       // export { foo, bar }
    NamedFrom { items: Vec<ExportItem>, path: String }, // export { foo } from "./mod"
    All(String),                  // export * from "./mod"
    Default(Box<Spanned<Item>>),  // export default expression
}

#[derive(Debug, Clone)]
pub struct ExportItem {
    pub name: String,
    pub alias: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub enum Type {
    Primitive(PrimitiveType),
    Path(String),
    Generic(String, Vec<Type>),
    Union(Vec<Type>),
    Intersection(Vec<Type>),
    Array(Box<Type>),
    Tuple(Vec<Type>),
    Option(Box<Type>),
    Function(Vec<Type>, Box<Type>),
}

#[derive(Debug, Clone)]
pub enum PrimitiveType {
    String,
    Number,
    Bool,
    Void,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Spanned<Statement>>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Let(LetStatement),
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub mutable: bool,
    pub name: String,
    pub ty: Option<Type>,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Identifier(String),
    Call(CallExpression),
    Member(MemberExpression),
    Match(MatchExpression),
    Block(Block),
    Binary(BinaryExpression),
    VariantCtor { enum_name: String, variant: String, args: Vec<Expression> },
    ObjectLiteral(Vec<ObjectField>),
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct MemberExpression {
    pub object: Box<Expression>,
    pub property: String,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
}

#[derive(Debug, Clone)]
pub struct ObjectField {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct MatchExpression {
    pub scrutinee: Box<Expression>,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expression>,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Identifier(String),
    Literal(Literal),
    Tuple(Vec<Pattern>),
    Variant(String, Vec<Pattern>),
    Wildcard,
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Number(String),
    Bool(bool),
}